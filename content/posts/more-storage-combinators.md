---
date: '2025-05-05T19:43:01+10:00'
draft: true
title: 'Matryoshka 2: More Storage Combinators'
tags:
- elixir
- programming
publishDate: 2025-05-20
---

Let's add some more stores and store combinators to Matryoshka, my composable storage library in Elixir.

## More store combinators

### MappingStore

[*Storage Combinators*](https://dl.acm.org/doi/10.1145/3359591.3359729) proposes a Mapping Store which applies transformations on references and values:

> The Mapping Store is an abstract superclass modelled after a map() function or a Unix filter, applying simple transformations to its inputs to yield its outputs when communicating with its source. Due to the fact that stores have a slightly richer protocol than functions or filters, the mapping store has to perform three separate mappings:
> 1. Map the reference before passing it to the source.
> 2. Map the data that is read from the source after it is read.
> 3. Map the data that is written to the source, before it is written.

This would be pretty useful for all sorts of deserialization / serialization stores: we could use regular functions to translate Elixir types back and forth into JSON (or XML, or S-expressions, or CSV..., ad infinitum) to store data on disk.

First things first, we'll define the module and a type to store the three mapping functions:

1. `map_ref` is a function that maps references `(ref -> mapped_ref)` before using them to locate values
2. `map_retrieved` is a function `(stored_value -> value)` that maps values when retrieved (get/fetch) from the store
3. `map_to_store` is a function `(value -> stored_value)` that maps values *before* storing them

I've only enforced the `inner` field for the struct. If the function isn't provided in the struct, I'll default to the identity function (which returns its input value unchanged), so the reference/value won't be transformed. This is a bit more convenient when defining mapping stores where we only want to map the reference, or only want to map the values on storage and retrieval.

```elixir {linenos=inline title="/lib/matryoshka/impl/mapping_store.ex"}
defmodule Matryoshka.Impl.MappingStore do
  alias Matryoshka.IsStorage
  alias Matryoshka.Storage
  alias Matryoshka.Reference
  
  @identity &Function.identity/1

  @enforce_keys [:inner]
  defstruct [
    :inner,
    map_ref: @identity,
    map_retrieved: @identity,
    map_to_store: @identity
  ]

  @type t :: %__MODULE__{
          inner: IsStorage.t(),
          map_ref: (Reference.t() -> Reference.t()),
          map_retrieved: (any() -> any()),
          map_to_store: (any() -> any())
        }
  ...
```

I'll also add a helper function `mapping_store/2` to create the struct. We can change the mapping functions in the MappingStore by providing the functions as keywords.

```elixir {linenos=inline linenostart=22 title="/lib/matryoshka/impl/mapping_store.ex"}
  ...
  def mapping_store(inner, opts \\ []) do
    IsStorage.is_storage!(inner)
    map_ref = Keyword.get(opts, :map_ref, @identity)
    map_retrieved = Keyword.get(opts, :map_retrieved, @identity)
    map_to_store = Keyword.get(opts, :map_to_store, @identity)

    %__MODULE__{
      inner: inner,
      map_ref: map_ref,
      map_retrieved: map_retrieved,
      map_to_store: map_to_store
    }
  end
  ...
```

Now we just need to define the Storage protocol for the module. Like all our storage combinators, we're calling the Storage functions on the inner store, but we also call the mapping functions where necessary, i.e.:

- mapping references with `map_ref/1`
- mapping retrieved values with `map_retrieved/1`
- mapping stored values with `map_to_store/1`

```elixir {linenos=inline linenostart=36 title="/lib/matryoshka/impl/mapping_store.ex"}
  ...
  alias __MODULE__

  defimpl Storage do
    def fetch(store, ref) do
      value =
        Storage.fetch(
          store.inner,
          store.map_ref.(ref)
        )
      value_new =
        case value do
          {:ok, value} -> {:ok, store.map_retrieved.(value)}
          error -> error
        end

      value_new
    end

    def get(store, ref) do
      value = 
        Storage.get(
          store.inner, 
          store.map_ref.(ref)
        )
      value_new =
        case value do
          nil -> nil
          value -> store.map_retrieved.(value)
        end

      value_new
    end

    def put(store, ref, value) do
      inner_new =
        Storage.put(
          store.inner,
          store.map_ref.(ref),
          store.map_to_store.(value)
        )

      %{store | inner: inner_new}
    end

    def delete(store, ref) do
      inner_new = 
        Storage.delete(
          store.inner, 
          store.map_ref.(ref)
        )
      %{store | inner: inner_new}
    end
  end
end
```

### SwitchingStore

*Storage Combinators* defines a switching store which distributes requests to subsidiary stores. In the [first post of this series](https://julianferrone.com/posts/composable-storage-elixir/), I mentioned that:

> In cases where we would use a scheme in a URI, we can simply use the first path segment

So the idea behind my implementation is as follows:

1. We'll peel off the first path segment of a reference
2. Then use that segment to choose which underlying store to access
3. Then hand the rest of the reference to that store to act as the key to store and retrieve values

We'll keep the stores in a map of strings to stores underneath a struct:

```elixir {linenos=inline title="/lib/matryoshka/impl/switching_store.ex"}
defmodule Matryoshka.Impl.SwitchingStore do
  alias Matryoshka.Impl.SwitchingStore
  alias Matryoshka.Reference
  alias Matryoshka.Storage
  alias Matryoshka.IsStorage

  @enforce_keys [:path_store_map]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          path_store_map: %{String.t() => IsStorage.t()}
        }

  def switching_store(path_store_map) when is_map(path_store_map) do
    %__MODULE__{
      path_store_map: path_store_map
    }
  end
  ...
```

Before defining the implementations for storage, let's get some helper functions defined.

I want a function to update the path store map whenever we update an inner store. This just needs to reach into the underlying `path_store_map` and put the updated store there:

```elixir {linenos=inline linenostart=19 title="/lib/matryoshka/impl/switching_store.ex"}
  ...
  alias __MODULE__

  def update_substore(store, sub_store, sub_store_ref) do
    store.path_store_map
    |> Map.put(sub_store_ref, sub_store)
    |> SwitchingStore.switching_store()
  end
  ...
```

I'd also like a function to split a reference into two references:

1. The first path segment
2. The rest of the path (i.e., the remaining path segments concatenated with a `/`)

```elixir {linenos=inline linenostart=27 title="/lib/matryoshka/impl/switching_store.ex"}
  ...
  def split_reference(ref) do
    [path_head | path_tail] = Reference.path_segments(ref)

    case path_tail do
      [] -> {:error, {:ref_path_too_short, ref}}
      path -> {:ok, path_head, Enum.join(path, "/")}
    end
  end
  ...
```

To make life more convenient, I also want a function which:

1. Retrieves the substore
2. Returns the split path for me

```elixir {linenos=inline linenostart=36 title="/lib/matryoshka/impl/switching_store.ex"}
  ...
  def locate_substore(store, ref) do
    with {:split_ref, {:ok, path_first, path_rest}} <-
           {:split_ref, SwitchingStore.split_reference(ref)},
         {:fetch_substore, {:ok, sub_store}} <-
           {:fetch_substore, Map.fetch(store.path_store_map, path_first)} do
      {:ok, sub_store, path_first, path_rest}
    else
      {:split_ref, error} -> error
      {:fetch_substore, :error} -> {:error, :no_substore}
    end
  end
  ...
```

Now with those helper functions out of the way, we can define the methods for Storage. The basic gist is the same across all the methods:

1. Locate the substore using the first part of the path
2. Direct the Storage calls to the substore
3. If the methods update a substore (i.e. put and delete), we update the substore and then update the map in the struct

```elixir {linenos=inline linenostart=48 title="/lib/matryoshka/impl/switching_store.ex"}
  ...
  defimpl Storage do
    def fetch(store, ref) do
      with {:locate, {:ok, sub_store, path_first, path_rest}} <-
             {:locate, SwitchingStore.locate_substore(store, ref)},
           {:fetch, {:ok, value}} <-
             {:fetch, Storage.fetch(sub_store, path_rest)} do
        {:ok, value}
      else
        {:locate, error} -> error
        {:fetch, error} -> error
      end
    end

    def get(store, ref) do
      with {:ok, sub_store, path_first, path_rest} <-
             SwitchingStore.locate_substore(store, ref) do
        value = Storage.get(sub_store, path_rest)
        value
      else
        _error -> nil
      end
    end

    def put(store, ref, value) do
      with {:ok, sub_store, path_first, path_rest} <-
             SwitchingStore.locate_substore(store, ref) do
        new_sub_store = Storage.put(sub_store, path_rest, value)
        SwitchingStore.update_substore(store, new_sub_store, path_first)
      else
        _ -> store
      end
    end

    def delete(store, ref) do
      with {:ok, sub_store, path_first, path_rest} <-
             SwitchingStore.locate_substore(store, ref) do
        new_sub_store = Storage.delete(sub_store, path_rest)
        SwitchingStore.update_substore(store, new_sub_store, path_first)
      else
        _ -> store
      end
    end
  end
end
```

### BackupStore

### CachingStore

Ah, but we run into an issue with CachingStore.

Thankfully, there's a simple fix: we require `fetch/2` and `get/2` to return

## Exposing to the outside world

Of course, now that we've defined our implementation logics, it's time to expose them in the Matryoshka module:

```elixir {linenos=inline linenostart=17 hl_lines=["3-5", 9, 11] title="/lib/matryoshka.ex"}
  ...
  # Business logic
  defdelegate backup_store(source_store, target_stores), to: BackupStore
  defdelegate caching_store(main_store), to: CachingStore
  defdelegate caching_store(main_store, fast_store), to: CachingStore
  defdelegate logging_store(store), to: LoggingStore
  defdelegate map_store(), to: MapStore
  defdelegate map_store(map), to: MapStore
  defdelegate mapping_store(store, opts), to: MappingStore
  defdelegate pass_through(store), to: PassThrough
  defdelegate switching_store(path_store_map), to: SwitchingStore
end
```

## Next steps

OK so, we've run into a bit of an issue when it comes to the usability of Matryoshka. We've got a bunch of storage combinators to add all sorts of functionality, which is great, but all our stores so far have been in-memory only; so we lose all the data when the store closes (i.e. because the store BEAM process terminates). But now that we've implemented CachingStore, we have the ability to cache data using a fast in-memory store and a source on-disk store. So I think it's high time we add stores that persist data to disk.

We'll be doing that in the next post in this series.

You can see the latest version of Matryoshka at [my GitHub](https://github.com/julianferrone/matryoshka).