---
date: '2025-05-05T19:46:58+10:00'
draft: false
title: 'Quick Tip: Separation of Concerns with Three Module Processes in Elixir'
tags:
- elixir
- programming
publishDate: '2025-09-05'
---

Most of the Elixir code I've seen typically splits processes into two modules:

1. Business logic
2. A GenServer that wraps the business logic with `handle_call`, `handle_cast`, `handle_info` implementations and helper functions (that, internally, use the `call` and `cast` functions to send messages to the server)

I've recently been using a tripartite model where I split that 2nd module into two parts, ending up with a module organization that looks like this:

| Module | Responsibility |
| --- | --- |
| Client | Provide convenience functions for sending messages to the GenServer (including initialisation, termination, etc.) in *Server*. |
| Server | Handle messages with a GenServer and call out to the the business logic in `Impl`. |
| Impl | Business logic - the bulk of the program. |

Let me illustrate with an example---a stack process that can:

- Push items onto a stack
- Pop items off the top of a stack (returning `:empty` if the stack is empty)
- Peek at the top item of the stack

We'll start with the `Stack.Impl` module. All we need to do is define regular functions for working with a stack:

```elixir
defmodule Stack.Impl do
  def pop([]), do: {:empty, []}
  def pop([head | stack]), do: {head, stack}

  def peek(stack = [head | _rest]), do: {head, stack}

  def push(value, stack), do: [value | stack]
end
```

Now that we have the business logic, we want to expose it through a GenServer so that we can run a process. The only responsibility of `Stack.Server` is to be a simple GenServer wrapper over our business logic---handling call/cast messages by matching them to our functions.

```elixir
defmodule Stack.Server do
  use GenServer

  alias Stack.Impl

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def init(_) do
    {:ok, []}
  end

  def handle_call(:pop, _from, stack) do
    {head, stack} = Impl.pop(stack)
    {:reply, head, stack}
  end

  def handle_call(:peek, _from, stack) do
    {head, stack} = Impl.peek(stack)
    {:reply, head, stack}
  end

  def handle_cast(:terminate, _stack) do
    exit(:terminate)
  end

  def handle_cast({:push, value}, stack) do
    {:noreply, Impl.push(value, stack)}
  end

  def terminate(reason, stack) do
    IO.puts("Terminating due to #{inspect(reason)} with current stack: #{inspect(stack)}")
  end
end
```

But it would be annoying to have to remember the messages we need to send to the server. That's why we provide helper functions for starting, stopping, and communicating with the server in `Stack.Client`:

```elixir
defmodule Stack.Client do
  @server Stack.Server

  def start_link() do
    start_link([])
  end

  def start_link(list) do
    Stack.Server.start_link(list)
  end

  def pop do
    GenServer.call(@server, :pop)
  end

  def peek do
    GenServer.call(@server, :peek)
  end

  def push(value) do
    GenServer.cast(@server, {:push, value})
  end

  def stop do
    GenServer.call(@server, :terminate)
  end
end
```

Users of the stack library will do all their interaction using the functions in `Stack.Client`.

This is how I organized [Matryoshka, a composable key-value storage Elixir library](https://github.com/julianferrone/matryoshka). 

Matryoshka shows another advantage of this approach; because each of my different stores adhere to the same Protocol, I could design `Matryoshka.Client` and `Matryoshka.Server` to work on the Protocol, not a specific implementation.

Then, I re-exported functions (via `defdelegate`) under the `Matryoshka` module for clients to use:

1. Functions from `Matryoshka.Client` for starting and communicating with storage servers
2. Functions from the various `Matryoshka.Impl.<Store>` modules for creating different (hence injecting different implementations of the Storage protocol into the storage server)

So really, I suppose the extended version of my suggestion adds another required module, the protocol, along with N different implementations:

| Module | Responsibility |
| --- | --- |
| Client | Provide convenience functions for sending messages to the GenServer (including initialisation, termination, etc.) in *Server*. |
| Server | Handle messages with a GenServer and call out to the the business logic in `Impl`. |
| Protocol | Specifies the API that should be defined in the different implementations. |
| Impl.A | One way of handling business logic. |
| Impl.B | Another way of handling business logic. |

But hey, "tripartite" has a good ring to it---and both "quadripartite" (to add the protocol module) and "qinquepartite" (to add the second implementation) suck.