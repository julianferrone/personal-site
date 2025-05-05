---
date: '2025-05-05T10:18:45+10:00'
draft: true
title: 'Matryoshka 1: Composable Storage in Elixir'
tags:
- elixir
- programming
---

I recently came across the paper [*Storage Combinators*](https://dl.acm.org/doi/10.1145/3359591.3359729), which proposes an approach to designing storage systems by composing together modular components.

The code in the paper is given in Objective Smalltalk, but I decided to build **Matryoshka**—inspired by *Storage Combinators*, but not an exact replica—in Elixir.

In the Storage Combinators paper, the composition of different stores and store combinators is done via message passing.

Originally, I chose Elixir as I thought this message passing style mapped nicely to the message passing and handling of GenServers.

After an early false start, I realised this approach had some issues:

- It would add another BEAM process for every underlying store in a composed store, as store combinator processes would need to send messages to other store processes
- The Client and Server modules were only ever intended to be thin wrapper functions around the business logic, and there would be massive duplication around those

I ended up refactoring so that store composition was done with regular structs and functions.

We first compose the business logic of a store together, before wrapping it in a Server and interacting with it via the Client module—so we only ever have 1 process per store, no matter how complex it is under the hood.

Regardless, the ideas around concurrency that the BEAM VM and BEAM languages (Erlang, Elixir, Gleam, Lisp Flavoured Erlang, etc.) offer are very interesting. A few examples of the design choices that come to mind:

- BEAM processes are lightweight (~2 Kb) green threads that are isolated from each other, so failures in one process don't crash other processes
- Round robin pre-emptive scheduling to ensure that no process can hog CPU usage
- Constructs like supervisors, which can restart failed processes from a good state

OK, so why the name "Matryoshka"?

In **Matryoshka**, I eventually elected to provide the composition functionality by wrapping stores with store combinators (more on this later).

This struct composition reminded me of Russian nesting dolls, which recursively store ever smaller Russian nesting dolls inside themselves.

## The storage protocol

*Storage Combinators* proposes a storage protocol that looks like this:

```smalltalk
protocol Storage {
  -at:ref.
  -<void>at:ref put:object.
  -<void>at:ref merge:object
  -<void>deleteAt:ref;
}
```

Any object that implements the Storage protocol is a **store**.

Some stores handle their storage calls (at/put/merge/deleteAt) by referring to other stores then processing those stores' results in some way (e.g. by combining, filtering, mapping). These stores are **store combinators**.

This is the main novelty of the paper.

So, there are four methods that have to be defined on an object for it to be a store:

1. `at` takes a reference and returns the data at that reference (if any)
2. `put` puts an object at the reference `ref`, and returns nothing
3. `merge` merges a provided object with the object at the reference `ref`, and returns nothing
4. `deleteAt` deletes the object at the reference `ref`

These four methods are equivalent to the 4 HTTP methods:

| Objective Smalltalk     | HTTP                   |
| ----------------------- | ---------------------- |
| store at:ref            | GET \<URI\>            |
| store at:ref put:data   | PUT \<URI\> \<data\>   |
| store at:ref merge:data | PATCH \<URI\> \<data\> |
| store deleteAt:ref      | DELETE \<URI\>         |

The paper's barely started but I've already decided to diverge in my implementation:

- I removed the `merge` method
- and I added a `fetch` method

### Removing merge

I removed `merge` as I want the storage combinators to be as generic as possible. The issue with patching-style methods is that not all types make sense to be patched—for data formats like JSON and XML, there's JSON Patch and XML Patch. But what merge operations are there for an atom or a string?

They're such small types it makes more sense to just put a new value there instead.

Also, you can create the patch functionality by first getting the value, updating the value, and then putting it in—so it's arguably not even a primitive function anyways.

However, I may end up adding an `update` function to the **Storage** protocol like [Map.update/4](https://hexdocs.pm/elixir/1.12/Map.html#update/4) which updates the value with a function.

Then if we want to apply a JSON Patch to a Map or List, we can create a 1-argument lambda by partially applying a JSON patch function with the desired patch operations, which will then be passed to `update`.

### Adding fetch

There are a few lookup methods in [Map](https://hexdocs.pm/elixir/1.12/Map.html), but the main basic methods are `get/2` and `fetch/2`, which differ in the shape of their return values:

| Method    | Return on success | Returns on failure |
| --------- | ----------------- | ------------------ |
| `get/2`   | `value`           | `nil`              |
| `fetch/2` | `{:ok, value}`    | `:error`           |

One of the nice things about `fetch/2` is that you can distinguish between no value being stored at a key (which is returned as `:error`) and the value `nil` being stored at a key (which is returned as `{:ok, nil}`).

`get/2` on the other hand returns `nil` in both cases.

I also decided to augment the return type of the errors in fetch with a reason—so the fetch in Matryoshka is intended to return either `{:ok, value}` when a value is stored at the requested key, or `{:error, reason}` if there's an error.

### Storage Protocol in Elixir

There's one last change that needs to be made when porting over the storage protocol to Elixir.

Elixir, as a functional programming language, has immutable data. To deal with changing state, we need a pure function which creates a new state from the old state.

The **Storage** protocol will specify these pure functions.

Later, we'll use a GenServer to track the changes in state.

As such, we write the storage protocol:

```elixir {linenos=inline title="/lib/matryoshka/storage.ex"}
defprotocol Matryoshka.Storage do
  alias Matryoshka.Reference

  @typedoc """
  A type that implements the Matryoshka.Storage protocol.
  """
  @type store :: any

  @type value :: any

  @doc """
  Fetches the value for a specific `ref` in `store`.

  If `store` contains the given `ref` then its value is returned in the 
  shape of `{:ok, value}`.
  If `store` doesn't contain `ref`, then the reason why is returned in the 
  shape of `{:error, reason}`.
  """

  def fetch(store, ref)

  @doc """
  Gets the value for a specific `ref` in `store`.

  If `store` contains the given `ref` then its value `value` is returned.
  If `store` doesn't contain `ref`, `nil` is returned.
  """
  @spec get(store(), Reference.impl_reference()) :: value()
  def get(store, ref)

  @doc """
  Puts the given `value` under `ref` in `store`.
  """
  @spec put(store(), Reference.impl_reference(), value()) :: store()
  def put(store, ref, value)

  @doc """
  Deletes the entry in `store` for a specific `ref`.

  If the `ref` does not exist, returns `store` unchanged.
  """
  @spec delete(store(), Reference.impl_reference()) :: store()
  def delete(store, ref)
end
```

And now we're ready to start putting together implementations for **Storage**.

As a matter of taste, I like a tripartite approach to building out processes in Elixir:

1. A `Client` module, which provides utility functions that wrap sending call and cast messages to the Server
2. A `Server` module, which handles calls and casts, but uses the business logic in Impl
3. An `Impl` module, which provides the actual business logic in pure functions

The `Client` and `Server` modules are therefore thin wrappers providing the concurrency, with the bulk of the code in `Impl`.

We'll start with the `Client` and `Server` code first, before diving into the nitty-gritty Impl variations.

## Implementation

### Client

The `Matryoshka.Client` module will abstract over sending messages to `Matryoshka.Server` by exposing functions to start the server and send storage messages:

```elixir {linenos=inline title="/lib/matryoshka/client.ex"}
defmodule Matryoshka.Client do
  @server Matryoshka.Server

  def start_link(store) do
    @server.start_link(store)
  end

  def get(ref) do
    GenServer.call(@server, {:get, ref})
  end

  def fetch(ref) do
    GenServer.call(@server, {:fetch, ref})
  end

  def put(ref, value) do
    GenServer.cast(@server, {:put, ref, value})
  end

  def delete(ref) do
    GenServer.cast(@server, {:delete, ref})
  end
end
```

The `:fetch` and `:get` messages will need to return the value (or an error/`nil`) so these are sent as synchronous calls.

Next we need to send the `:put` and `:delete` messages. I've elected to send these as casts rather than as calls.

I'm aware that calls in Elixir provide useful backpressure since they're synchronous, whereas the casts (asynchronous) don't, but I'm not planning on using **Matryoshka** in a production context anyways.

Now it's time to handle the storage messages.

### Server

The `MapStore.Server` module only needs to be a thin GenServer wrapper around the business logic in `MapStore.Impl`, so we start with `start_link/1` to and `init/1` to initialize the server:

```elixir {linenos=inline title="/lib/matryoshka/mapstore/server.ex"}
defmodule Matryoshka.Server do
  use GenServer

  alias Matryoshka.Storage

  def start_link(default) do
    GenServer.start_link(__MODULE__, default, name: __MODULE__)
  end

  @impl true
  def init(store) do
    {:ok, store}
  end
  ...
```

The `store` provided in `init/1` here is any struct which implements the **Storage** protocol.

You'll also notice that the name is always set to `Server` when we start the store server.

Initially I thought that we'd only ever have 1 store running in the application (since there are store combinators which can deal with an arbitrarily large list of underlying stores), but I'll probably end up changing this (and `Client`) to request a name keyword eventually.

That way, if this were to run as an actual storage backend, we can do things like having a new store process per each user connected to the frontend application.

OK, so now we need to handle the different storage messages.

These just call out to the business logic in the provided store, using the **Storage** protocol to dispatch the function calls:

```elixir {linenos=inline linenostart=14 title="/lib/matryoshka/mapstore/server.ex"}
  ...
  @impl true
  def handle_call({:get, ref}, _from, store) do
    value = Storage.get(store, ref)
    {:reply, value, store}
  end

  @impl true
  def handle_call({:fetch, ref}, _from, store) do
    value = Storage.fetch(store, ref)
    {:reply, value, store}
  end

  @impl true
  def handle_cast({:put, ref, value}, store) do
    {:noreply, Storage.put(store, ref, value)}
  end

  @impl true
  def handle_cast({:delete, ref}, store) do
    {:noreply, Storage.delete(store, ref)}
  end
end
```

Again, `Server` and `Client` are just very thin wrappers around handling state and storage requests.

Now it's time to write some actual business logic.

### MapStore

**MapStore** is the simplest store we can make—an in-memory, map-backed store.

First we create a **MapStore** struct in the `MapStore` module with only one key, `map`, which will contain the underlying map:

```elixir {linenos=inline title="/lib/matryoshka/impl/map_store.ex"}
defmodule Matryoshka.Impl.MapStore do
  @enforce_keys [:map]
  defstruct [:map]
  ...
```

Along with a helper function to create the **MapStore** struct from a map:

- `map_store/0` creates a **MapStore** from a new map
- `map_store/1` creates a **MapStore** from the provided map

```elixir {linenos=inline linenostart=4 title="/lib/matryoshka/impl/map_store.ex"}
  ...
  def map_store(), do: map_store(Map.new())
  def map_store(map), do: %__MODULE__{map: map}
  ...
```

Then we need to define the four methods to implement the Storage protocol.

`Map.fetch/2` only returns an `:error` in the case of the key not being found, but as mentioned earlier, I want to add a reason when there's a failure. I provide that in the shape `{:no_ref, ref}`, which means "No reference in the store for the provided `ref`":

```elixir {linenos=inline linenostart=7 title="/lib/matryoshka/impl/map_store.ex"}
  ...
  alias __MODULE__

  defimpl Matryoshka.Storage do
    def fetch(store, ref) do
      case Map.fetch(store.map, ref) do
        {:ok, value} -> {:ok, value}
        :error -> {:error, {:no_ref, ref}}
      end
    end
    ...
```

`get/2` is a simple call to `Map.get/2`:

```elixir {linenos=inline linenostart=17 title="/lib/matryoshka/impl/map_store.ex"}
    ...
    def get(store, ref), do: Map.get(store.map, ref)
    ...
```

`put/3` and `delete/2` are similarly simple calls to `Map.put/3` and `Map.delete/2`, although we need to wrap the underlying map back into a **MapStore**:

```elixir {linenos=inline linenostart=19 title="/lib/matryoshka/impl/map_store.ex"}
# /lib/matryoshka/impl/map_store.ex
    ...
    def put(store, ref, value) do
      map = Map.put(store.map, ref, value)
      Impl.map_store(map)
    end

    def delete(store, ref) do
      map = Map.delete(store.map, ref)
      Impl.map_store(map)
    end
  end
end
```

...and that completes the business logic for `MapStore`!

### PassThrough

*Storage Combinators* defines a pass-through store **PassThrough**, which is the simplest store combinator—it just passes all its requests to its source store, unchanged.

In *Storage Combinators*, **PassThrough** is a useful object as other store combinators can inherit from it, reusing the functionality of passing storage requests through to other stores.

In **Matryoshka**, it's completely useless, as there's no such thing as class inheritance in Elixir. We'll need to write the code for storage combinators passing requests to their inner stores by hand.

But we'll build it anyways just to get a feel for what a **storage combinator** looks like.

Just like with `MapStore`, we start with a struct and a helper function to construct the struct:

```elixir {linenos=inline title="/lib/matryoshka/impl/pass_through.ex"}
defmodule Matryoshka.Impl.PassThrough do
  alias Matryoshka.Storage
  
  @enforce_keys [:inner]
  defstruct [:inner]

  @typedoc """
  A struct that implements the Matryoshka.Storage protocol.
  """
  @type impl_storage :: any()

  @type t :: %__MODULE__{
          inner: impl_storage
        }

  def pass_through(storage) when is_struct(storage) do
    %__MODULE__{inner: storage}
  end
  ...
```

and then implement the **Storage** protocol methods.

`fetch/2` and `get/2` just call into the inner store:

```elixir {linenos=inline linenostart=19 title="/lib/matryoshka/impl/pass_through.ex"}
  ...
  alias __MODULE__

  defimpl Storage do
    def fetch(store, ref), do: Storage.fetch(store.inner, ref)

    def get(store, ref), do: Storage.get(store.inner, ref)
    ...
```

While `put/3` and `delete/2` call into the inner store, then re-wrap it in a `PassThrough` struct.

```elixir {linenos=inline linenostart=26 title="/lib/matryoshka/impl/pass_through.ex"}
    ...
    def put(store, ref, value) do
      new_inner = Storage.put(store.inner, ref, value)
      PassThrough.pass_through(new_inner)
    end

    def delete(%PassThrough{inner: inner}, ref) do
      new_inner = Storage.delete(inner, ref)
      PassThrough.pass_through(new_inner)
    end
  end
end
```

This pattern of calling into the inner structs, then re-wrapping the changed stores, is the defining characteristic of the storage combinators.

### LoggingStore

Now it's time to construct a useful storage combinator.

*Storage Combinators* defines a logging store that sends a log message when a storage call is made, but delegates the actual getting/fetching/putting/deleting of values to the inner store.

We'll do the same.

Once again we start with a struct and a helper function to create the struct:

```elixir {linenos=inline title="/lib/matryoshka/impl/logging_store.ex"}
defmodule Matryoshka.Impl.LoggingStore do
  alias Matryoshka.Storage
  @enforce_keys [:inner]
  defstruct [:inner]

  @typedoc """
  A struct that implements the Matryoshka.Storage protocol.
  """
  require Logger
  @type impl_storage :: any()

  @type t :: %__MODULE__{
          inner: impl_storage
        }

  def logging_store(storage) when is_struct(storage) do
    %__MODULE__{inner: storage}
  end
  ...
```

And then we'll define the four **Storage** functions—`fetch/2`, `get/2`, `put/3`, and `delete/2`.

`fetch/2` and `get/2` will once again compute their results by just calling to the inner store using Storage. We'll then log the results using structured log messages.

```elixir {linenos=inline linenostart=19 title="/lib/matryoshka/impl/logging_store.ex"}
  ...
  alias __MODULE__

  defimpl Storage do
    def fetch(store, ref) do
      value = Storage.fetch(store.inner, ref)
      Logger.info([request: "FETCH", ref: ref, value: value])
      value
    end

    def get(store, ref) do
      value = Storage.get(store.inner, ref)
      Logger.info([request: "GET", ref: ref, value: value])
      value
    end
    ...
```

We implement `put/3` and `delete/2` to:

1. Log a message
2. Change the inner store using the function dispatch of Storage
3. Wrap the changed inner store in a LoggingStore

```elixir {linenos=inline linenostart=34 title="/lib/matryoshka/impl/logging_store.ex"}
    ...
    def put(store, ref, value) do
      Logger.info([request: "PUT", ref: ref, value: value])
      inner = Storage.put(store.inner, ref, value)
      LoggingStore.logging_store(inner)
    end

    def delete(store, ref) do
      Logger.info([request: "DELETE", ref: ref])
      inner = Storage.delete(store.inner, ref)
      LoggingStore.logging_store(inner)
    end
  end
end
```

And LoggingStore is done.

### Exposing functionality via an external module

As a convenience, I re-export all the functionality from `Client`, `Server`, and the various implementation modules in the `Matryoshka` module via the `defdelegate` macro:

```elixir {linenos=inline title="/lib/matryoshka.ex"}
defmodule Matryoshka do

  # Implementation logic
  alias Matryoshka.Impl.LoggingStore
  alias Matryoshka.Impl.MapStore
  alias Matryoshka.Impl.PassThrough

  # Client to interact with store
  alias Matryoshka.Client

  # Client
  defdelegate start_link(store), to: Client
  defdelegate get(ref), to: Client
  defdelegate fetch(ref), to: Client
  defdelegate put(ref, value), to: Client
  defdelegate delete(ref), to: Client

  # Business logic
  defdelegate logging_store(store), to: LoggingStore

  defdelegate map_store(), to: MapStore
  defdelegate map_store(map), to: MapStore

  defdelegate pass_through(store), to: PassThrough
end
```

This just allows developers to use **Matryoshka** in a more ergonomic way, by importing or aliasing only this one module, hiding the implementation logic.

## Matryoshka in action

Now that we've got some stores and store combinators defined, we can finally try composing a store, and then updating and querying it:

```elixir
alias Matryoshka

{:ok, client} =
  Matryoshka.map_store()
  |> Matryoshka.logging_store()
  |> Matryoshka.start_link()
#=> {:ok, #PID<0.152.0>}

Matryoshka.put("key", :value)
#=> 10:20:30.000 [info] [request: "PUT", ref: "key", value: :value]
#=> :ok

Matryoshka.fetch("key")
#=> 10:20:35.000 [info] [request: "FETCH", ref: "key", value: {:ok, :value}]
#=> {:ok, :value}

Matryoshka.delete("key")
#=> 10:20:40.000 [info] [request: "DELETE", ref: "key"]
#=> :ok

Matryoshka.get("key")
#=> 10:20:45.000 [info] [request: "GET", ref: "key", value: nil]
#=> nil
```

In the next post, I'll add some more stores and store combinators to **Matryoshka** to make it more useful.

You can see the latest version of **Matryoshka** at [my GitHub](github.com/julianferrone/matryoshka).

## References

Weiher, M., & Hirschfeld, R. (2019). **Storage combinators**. _Proceedings of the 2019 ACM SIGPLAN International Symposium on New Ideas, New Paradigms, and Reflections on Programming and Software_, 111–127. [![DOI:10.1145/3359591.3359729](https://camo.githubusercontent.com/c457178dd736b60872309d95720942554912fad902d90a11868cd47a914c046a/68747470733a2f2f7a656e6f646f2e6f72672f62616467652f444f492f31302e313134352f333335393539312e333335393732392e737667)](https://doi.org/10.1145/3359591.3359729)
