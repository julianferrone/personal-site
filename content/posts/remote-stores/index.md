---
date: '2025-05-07T10:42:02+10:00'
draft: false
title: 'Matryoshka 4: Remote Stores'
tags:
- elixir
- programming
publishDate: '2025-11-29'
series: matryoshka
---

In the last post, I wrote some stores which persist their key-value pairs to disk.

In the spirit of gross overcomplication{{< sidenote >}}I suppose there is a reasonable use-case here of using a remote store---e.g. S3---as a backup.{{< /sidenote >}}, let's add a remote store that reads and writes by calling out to an external server. Because there's nothing like [adding an extra 100 ms of latency](https://github.com/sirupsen/napkin-math) for no reason other than upgrading your architecture to the paradigm du jour.

Remember, your app isn't Real Software unless it uses Microservices™.

Originally, I wanted to write a `HttpStore` that would map get/fetch/put/delete calls to HTTP GET/POST/DELETE requests, but I don't think it's possible to make it generic since you'd have to write the implementation against the specific API requirements{{< sidenote >}}Speaking of S3, I probably should've built my HttpStore to expect a server that exposes the same interface as [the S3 REST API](https://docs.aws.amazon.com/AmazonS3/latest/API/RESTAPI.html). Perhaps I'll implement `Matryoshka.S3Store` one day.{{< /sidenote >}} of your key-value server:

- Does the KV server expose a version? How so?
  - Is it `/v1/`?
  - Or `/2.0/`?
  - Or `/20251128/`?
  - Or `/api/v4/`?
  - Or something entirely different?
- Which routes does the KV server expose? What's the namespace? Is it `/kv/`, or is it `/store/`, or is it something else?
- Does the KV server even expose the difference between a key being set to a null value (which should be returned as `{:ok, nil}` by `fetch/1`) and a key not being set at all (which should be returned as `{:error, {:no_ref, ref}}` by `fetch/1`)?

So `HttpStore` has been left as an exercise for the reader.

Instead, I'll write a remote store backed by an SFTP client---a `SftpStore`.

Why SFTP?

For starters, I deal with it all the time at my job at [S&P Financial Risk Analytics](https://www.spglobal.com/market-intelligence/en/solutions/financial-risk-analytics).  For some of our clients, we offer a risk pricing service where they ship us their data (their portfolio, counterparties, and other reference data) over SFTP, we run the pricing, and then we upload the risk results back to SFTP for them to use.{{< sidenote >}}This is typically driven either by A\) regulatory requirements, or B\) wanting a more accurate understanding of market exposures so they can trade better.{{< /sidenote >}}

Also, since SFTP is a file-storage protocol, an SFTP client-backed Matryoshka store will behave identically across different backend servers, so I don't have to worry about the same bikeshedding issues that `HttpStore` would've brought up.

## Implementing SftpStore

The `SftpStore` is basically the same as a [FilesystemStore](/persisting-data-to-disk/#filesystemstore) in that:

- It reads and writes values to their own files
- References are treated as file location paths

Of course, the big difference is that instead of writing to a local filesystem, we'll be writing to a remote one over SFTP.

I've had a minor goal of writing Matryoshka without external dependencies, and pleasantly, we can still achieve it; erlang has a built-in `ssh` application which implements both clients and daemons (servers, which we'll need for testing later).

So first things first, I'll need to update the Matryoshka mix project script to start up the `:ssh` application:

```elixir {title="/mix.exs"}
  def application do
    [
      extra_applications: [:logger, :ssh]
    ]
  end
```

And then we can get into implementing the actual store. That means we'll need to be able to:

1. Connect to an SFTP server
2. Read and write files over the connection

The `:ssh_sftp` module (which implements an SFTP client) [documentation](https://www.erlang.org/docs/28/apps/ssh/ssh_sftp#start_channel/1) tells us that we're looking for the function `start_channel` to connect to an SFTP server:

>```erlang
>-spec start_channel(ssh:host(), inet:port_number(), [ssh:client_option() | sftp_option()]) -> {ok, pid(), ssh:connection_ref()} | {error, reason()}.
>```
>
>Starts new ssh connection and channel for communicating with the SFTP server.
>
>The returned pid for this process is to be used as input to all other API functions in this module.

OK, so we'll need to provide this with a host and port number to connect to an SFTP server. Easy enough.

But it'd also be nice to be able to log into private servers using a username and password.{{< sidenote >}}Authentication via private key ALSO left as an exercise to the reader.{{< /sidenote >}}

It turns out one of the `ssh:client_option()` is `authentication_client_options()`, which is exactly what we're looking for:

> ```erlang
> -type authentication_client_options() :: {user, string()} | {password, string()}.
> ```
>
> **`user`** - Provides the username. If this option is not given, `ssh` reads from the environment (LOGNAME or USER on UNIX, USERNAME on Windows).
>
> **`password`** - Provides a password for password authentication. If this option is not given, the user is asked for a password, if the password authentication method is attempted.

And so first things first, we'll want to initialise an `SftpStore` by starting a connection to the SFTP server (passing username and password), then saving the PID and connection in a struct.

```elixir {linenos=inline linenostart=1 title="/lib/matryoshka/impl/sftp_store.ex"}
defmodule Matryoshka.Impl.SftpStore do
  alias Matryoshka.Reference
  @enforce_keys [:pid, :connection]
  defstruct [:pid, :connection]
  alias __MODULE__

  @type t :: %SftpStore{
          pid: pid(),
          connection: :ssh.connection_ref()
        }

  def sftp_store(host, port, username, password) do
    # Since we're dealing with `:ssh`, which is an Erlang 
    # module, we'll need to convert the username and password
    # from Strings to charlists.
    username = String.to_charlist(username)
    password = String.to_charlist(password)

    :ssh.start()

    {:ok, pid, connection} =
      :ssh_sftp.start_channel(
        host,
        port,
        silently_accept_hosts: true,
        user: username,
        password: password
      )

    %SftpStore{pid: pid, connection: connection}
  end
```

Just like with `FilesystemStore`, we need to deal with nested keys like `foo/bar`, which we'll do by treating the prefixes as directories, and the final path segment as a filename. So we'll need to recursively make directories -- but annoyingly, the `:ssh_sftp` function `make_dir/2` requires that "the directory can only be created in an existing directory", so I'll need to write the recursion myself.

Given some key like `"foo/bar/baz`", we want to convert it into a list of nested directories, where each item in the list is the child of the previous item:

```elixir
iex> Reference.path_segments("foo/bar/baz") |> parent_dirs()
["foo", "foo/bar"]
```

```elixir {linenos=inline linenostart=33 title="/lib/matryoshka/impl/sftp_store.ex"}
  def parent_dirs(path_segments) do
    # This function lets us pull all the parents from a 
    # path reference, so that we can make them in the
    # underlying SFTP directory.
    {paths, _acc} =
      path_segments
      # We don't want to make the last path segment 
      # as a directory, since that'll be the filename.
      |> Enum.drop(-1)
      |> Enum.map_reduce(
        [],
        fn segment, acc -> {
          [segment | acc], # Applied to the segment
          [segment | acc]  # Applied to the accumulator
        } end
      )

    paths
    # Reverse the paths since we've been prepending
    # the children to their parents
    |> Enum.map(&Enum.reverse/1)
    # Then recombine them into paths with 
    # forward-slash delimiters
    |> Enum.map(&Enum.join(&1, "/"))
  end
```

And after that, implementing the rest of the Storage protocol is a breeze.

`fetch/1` and `get/1` just need to read the file from the SFTP server and return the results:

```elixir {linenos=inline linenostart=58 title="/lib/matryoshka/impl/sftp_store.ex"}
  defimpl Matryoshka.Storage do
    def fetch(store, ref) do
      value =
        case :ssh_sftp.read_file(
               store.pid,
               String.to_charlist(ref)
             ) do
          {:ok, bin} -> {:ok, :erlang.binary_to_term(bin)}
          {:error, :no_such_file} -> {:error, {:no_ref, ref}}
          {:error, other} -> {:error, other}
        end

      {store, value}
    end

    def get(store, ref) do
      ref = String.to_charlist(ref)

      value =
        case :ssh_sftp.read_file(store.pid, ref) do
          {:ok, bin} -> :erlang.binary_to_term(bin)
          {:error, _reason} -> nil
        end

      {store, value}
    end
```

`put/1` needs to ensure that the parent directories exist, before writing the value to the SFTP as a file:

```elixir {linenos=inline linenostart=80 title="/lib/matryoshka/impl/sftp_store.ex"}
    def put(store, ref, value) do
      # Make sure that parent directories exist
      segments = Reference.path_segments(ref)

      if length(segments) > 1 do
        dirs = SftpStore.parent_dirs(segments)

        Enum.each(
          dirs,
          fn dir -> :ssh_sftp.make_dir(store.pid, dir) end
        )
      end

      # Write value
      :ssh_sftp.write_file(
        store.pid,
        String.to_charlist(ref),
        :erlang.term_to_binary(value)
      )

      store
    end
```

And `delete/1` just needs to ask the SFTP server to delete the file:

```elixir {linenos=inline linenostart=103 title="/lib/matryoshka/impl/sftp_store.ex"}
    def delete(store, ref) do
      :ssh_sftp.delete(
        store.pid,
        String.to_charlist(ref)
      )

      store
    end
  end
end
```

And with that, the implementation of `SftpStore` is finished.

## Testing SftpStore

Let's briefly discuss the [testing suite for SftpStore](https://github.com/julianferrone/matryoshka/blob/main/test/impl/sftp_store_test.exs). `:ssh` exposes a `daemon/3` function that [lets us start an SFTP server](https://www.erlang.org/docs/28/apps/ssh/ssh.html#daemon/3):

>```erlang
>-spec daemon(any | inet:ip_address(), inet:port_number(), daemon_options()) -> {ok, daemon_ref()} | {error, term()};
>(socket, open_socket(), daemon_options()) -> {ok, daemon_ref()} | {error, term()}. 
>```
>
>Starts a server listening for SSH connections on the given port. If the Port is 0, a random free port is selected. See daemon_info/1 about how to find the selected port number.

So in testing, I initialise an SFTP server with username "user", password "password", and a subsystem specification of `:ssh_sftpd`, which tells the `:ssh` daemon to act as an SFTP filesystem{{< sidenote >}}An `:ssh` daemon uses both generic SSH channel functionality (e.g. flow control, close messages) provided by `:ssh_server_channel` and application-specific functionality (here, reading and writing files) which get used via a callback API.

This is classic Erlang style programming.

Another good example is GenServers, which implement generic server functionality, and which you specialise by writing callback handlers (via `handle_call/3` and `handle_cast/2`) for application-specific functionality.{{< /sidenote >}}. Then we connect to it with a `SftpStore` and test get/fetch/put/delete.

This is a real SFTP server that we're connecting to.

No mocking necessary.

```elixir {linenos=inline linenostart=1 title="/test/impl/sftp_store_test.exs"}
defmodule MatryoshkaTest.SftpStoreTest do
  alias Matryoshka.Impl.SftpStore
  alias Matryoshka.Storage

  use ExUnit.Case, async: true

  # When the port is zero, the ssh daemon picks a random free port
  @random_port 0
  @user "user"
  @password "password"

  @moduletag :tmp_dir

  setup context do
    # Set up SFTP server options

    # Where the public keys are saved
    {:ok, cwd} = File.cwd()

    system_dir =
      to_charlist(
        Path.join([
          cwd,
          "test",
          "ssh"
        ])
      )

    user = String.to_charlist(@user)
    password = String.to_charlist(@password)
    root = String.to_charlist(context.tmp_dir)

    options = [
      system_dir: system_dir,
      user_passwords: [
        {user, password}
      ],
      subsystems: [
        :ssh_sftpd.subsystem_spec(root: root)
      ]
    ]

    # Start SFTP server
    :ssh.start()
    {:ok, server_ref} = :ssh.daemon(
      :loopback, 
      @random_port, 
      options
    )
    {:ok, daemon_info} = :ssh.daemon_info(server_ref)
    ip = Keyword.get(daemon_info, :ip)
    port = Keyword.get(daemon_info, :port)

    # Start SftpStore (SFTP Client)
    sftp_store = SftpStore.sftp_store(ip, port, @user, @password)

    # Close SFTP server when test is done
    on_exit(fn ->
      :ssh.stop_daemon(server_ref)
    end)

    {:ok, store: sftp_store}
  end
```

## Matryoshka is Published

There's plenty of extra work that I could do to improve Matryoshka.

I could write a version of SftpStore that used append-only writes, like [my LogStore](/persisting-data-to-disk/#logstore).

I could add a more performant, idiomatic in-memory store backed by ETS.

I could add stores backed by SQL databases---in both SQLite and Postgres flavours!

I could add a bunch more functions to the stores: `keys/1` to return all the keys in a store, `filter/2` to return all the key-value pairs where a predicate function returns a truthy value, or `update/4` to update a key in the store with a given function.{{< sidenote >}}Implementing almost all the functions in [Map](https://hexdocs.pm/elixir/Map.html) across the different Stores would be a great idea.{{< /sidenote >}}

And there's an infinite variety of storage combinators to implement, especially specialisations of MappingStore that translate terms into data transport formats: like a JSON mapping store, a XML mapping store, or even a [Universal Binary Format](https://ubf.github.io/ubf/ubf-user-guide.en.html) mapping store.

But I'm at a stage where I'm happy with Matryoshka, so I'm pleased to announce that I've finally pulled the trigger and [published it to Hex](https://hex.pm/packages/matryoshka/0.1.0), so now you too can use my ~~utterly ridiculous~~ ~~not production-ready~~ ~~experimental~~ innovative key-value storage tech in your projects.

As always, you can see the latest version of Matryoshka at [my GitHub](https://github.com/julianferrone/matryoshka).
