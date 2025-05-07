---
date: "2025-05-05T19:44:26+10:00"
draft: true
title: "Matryoshka 3: Persisting Data to Disk"
tags:
- elixir
- programming
publishDate: 2025-06-09
---

We left off in the last post having written a few extra storage combinators, including a CachingStore that lets us check multiple stores (one as a cache store, one as a main source-of-truth store). But all our stores are in-memory only, which means we'll lose all our data when the store processes finish.

Let's fix that by writing some stores that persist their data to disk. We'll start with a naive implementation, FilesystemStore, before building the much more sophisticated LogStore.

## FilesystemStore

In the spirit of "make it work, then make it better", we'll write a super naive disk store with FilesystemStore:

- Every value is stored under its own file
- The reference are treated as file location paths, relative to some given root directory

Let's start with a struct and constructor function:

```elixir {linenos=inline title="/lib/matryoshka/impl/filesystem_store.ex"}
defmodule Matryoshka.Impl.FilesystemStore do
  alias Matryoshka.Reference
  alias Matryoshka.Storage

  @enforce_keys [:root_dir]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          root_dir: Path.t()
        }

  @spec filesystem_store(Path.t()) :: t()
  def filesystem_store(root_dir) do
    %__MODULE__{root_dir: root_dir}
  end
  ...
```

Since the provided refs are going to be considered as paths relative to the root directory of the filesystem store, let's build a function to turn these paths into absolute paths:

```elixir {linenos=inline linenostart=16 title="/lib/matryoshka/impl/filesystem_store.ex"}
    ...
  @spec absolute_path(t(), Reference.t()) :: Path.t()
  def absolute_path(store, ref) do
    path_segments = [store.root_dir | Reference.path_segments(ref)]
    Path.join(path_segments)
  end
  ...
```

Then we can implement the Storage methods by reading from and writing to files:

```elixir {linenos=inline linenostart=22 title="/lib/matryoshka/impl/filesystem_store.ex"}
  alias __MODULE__

  defimpl Storage do
    def fetch(store, ref) do
      path = FilesystemStore.absolute_path(store, ref)

      with {:ok, value} <- File.read(path) do
        {store, {:ok, value}}
      else
        {:error, _reason} -> {store, {:error, {:no_ref, ref}}}
      end
    end

    def get(store, ref) do
      path = FilesystemStore.absolute_path(store, ref)

      with {:ok, value} <- File.read(path) do
        {store, value}
      else
        {:error, _reason} -> {store, nil}
      end
    end

    def put(store, ref, value) when is_binary(value) do
      path = FilesystemStore.absolute_path(store, ref)
      parent_dir = Path.dirname(path)
      File.mkdir_p(parent_dir)
      File.write(path, value)

      store
    end

    def delete(store, ref) do
      path = FilesystemStore.absolute_path(store, ref)
      _result = File.rm(path)
      store
    end
  end
end
```

However, there's a few issues with FilesystemStore. For a start, it only works with stringified keys and values. If we want to use it to store maps, lists, even integers, we'd need to compose it with a MappingStore. Also, it uses a new file for every value, which seems very wasteful. Perhaps this would be useful for storing extremely large values (on the order of megabytes), but not for 

We'll build LogStore with these limitations in mind.

## LogStore

*Firstly, I'd like to note that the code in this implementation is heavily inspired by the approach in [Build a Simple Persistent Key-Value Store in Elixir, using Logs – Part 2](https://www.poeticoding.com/build-a-simple-persistent-key-value-store-in-elixir-using-logs-part-2/). I've made some changes to deal with arbitrary keys and values, along with deleting values, but this article helped me immensely in understanding how append-only log-backed key-values stores work, along with giving me ideas on how to structure my log entries.*

The secret sauce to LogStore is `:erlang.term_to_binary/1` and `:erlang.binary_to_term/1`, which encode and decode Erlang (and Elixir) terms to and from binary. This is going to let us serialize any kinds of references and values we want to the log file, which fixes the first issue with FilesystemStore (only being able to read and write with string references and string values).

Whenever we make an update (put or delete) to LogStore, we'll append a log entry, in binary encoding, to the log file. We'll store the position and size of the value in an index map, which will allow us to read exactly `size` bytes from the file every time we want to retrieve (get or fetch) a value. All the 

LogStore is a lot more complicated than all the other stores we've written, so I've elected to break it up into four modules:

| Module | Explanation |
| --- | --- |
| Encoding | Defines how entries in the log file are stored. |
| Deserialize | Defines how to read data from the log file. |
| Serialize | Defines how to write data to the log file. |
| LogStore | Provides Storage capability by translating storage calls (put, get, fetch, delete) into reads from and writes to the log file. |

### Encoding

The Encoding module is mainly for bookkeeping.

Before we get into the code, let's work out how we're going to format our log entries. We'll have two kinds of log entries: writes, and deletes.

Both kinds of log entries will start with a timestamp. The next part of our entry is going to be a one-letter atom which represents whether the rest of the log entry is a write entry (`:w`) or a delete entry (`:d`).

The next entry will need to be a size indicator. If the log entry is a write, we'll need to store both the size of the key and the size of the value. If the log entry is a delete, we'll only need to store the size of the key.

The final entry will be the data:

- Write entries will include the key and the value
- Delete entries will include only the key

IMAGES

We'll store timestamps in a 64-bit unsigned int, which is large enough to hold a millisecond-precision Unix timestamp:

```elixir {linenos=inline title="/lib/matryoshka/impl/log_store/encoding.ex"}
defmodule Matryoshka.Impl.LogStore.Encoding do
  @timestamp_bitsize 64
  def timestamp_bitsize, do: @timestamp_bitsize
  
  @time_unit :millisecond
  def time_unit, do: @time_unit
  ...
```

We also provide functions in Encoding so that the Deserialize and Serialize modules can access the module attributes that we're defining.

We'll define the maximum key and value lengths to be 2^16 bits (~66 kB) and 2^32 bits (~4.3 GB) respectively, so we'll store the key and value lengths in a 16-bit unsigned int and a 32-bit unsigned int.

```elixir {linenos=inline linenostart=7 title="/lib/matryoshka/impl/log_store/encoding.ex"}
  ...
  @key_bitsize 16
  def key_bitsize, do: @key_bitsize

  @value_bitsize 32
  def value_bitsize, do: @value_bitsize
  ...
```

Since we're using single-letter atoms to represent write vs. delete entries in the log file, we need 4 bytes to store them (single-letter atoms have a length of 4 after converting them to binary with :erlang.term_to_binary/1):


```elixir {linenos=inline linenostart=13 title="/lib/matryoshka/impl/log_store/encoding.ex"}
  ...
  @atom_bytesize 4
  def atom_bytesize, do: @atom_bytesize

  @atom_write :w
  def atom_write, do: @atom_write
  def atom_write_binary, do: :erlang.term_to_binary(@atom_write)

  @atom_delete :d
  def atom_delete, do: @atom_delete
  def atom_delete_binary, do: :erlang.term_to_binary(@atom_delete)
  ...
```

We also need a helper function to calculate how many bits are in a byte, which will be useful for calculating entry sizes:

```elixir {linenos=inline linenostart=24 title="/lib/matryoshka/impl/log_store/encoding.ex"}
  ...
  def bits_to_bytes(bits), do: div(bits, 8)
  ...
```



```elixir {linenos=inline linenostart=26 title="/lib/matryoshka/impl/log_store/encoding.ex"}
  def delete_entry_size(key_size) do
    Enum.sum([
      bits_to_bytes(@timestamp_bitsize),
      atom_bytesize(),
      bits_to_bytes(@key_bitsize),
      key_size
    ])
  end

  def write_entry_size(key_size, value_size) do
    Enum.sum([
      bits_to_bytes(@timestamp_bitsize),
      atom_bytesize(),
      bits_to_bytes(@key_bitsize),
      bits_to_bytes(@value_bitsize),
      key_size,
      value_size
    ])
  end

  def write_entry_pre_value_size(key_size) do
    Enum.sum([
      bits_to_bytes(@timestamp_bitsize),
      atom_bytesize(),
      bits_to_bytes(@key_bitsize),
      bits_to_bytes(@value_bitsize),
      key_size
    ])
  end
end

```

### Serialize

### Deserialize

### Wrapping into LogStore

### Packaging into PersistentStore

PersistentStore as example of pre-composed store

## Exposing to Matryoshka consumers 

Now that we've written the business logic for FilesystemStore, LogStore, and PersistentStore, it's time to expose them in the Matryoshka module so users of our library can consume them:

```elixir {linenos=inline linenostart=17 hl_lines=[6, 8, 13] title="/lib/matryoshka.ex"}
  ...
  # Business logic
  defdelegate backup_store(source_store, target_stores), to: BackupStore
  defdelegate caching_store(main_store), to: CachingStore
  defdelegate caching_store(main_store, cache_store), to: CachingStore
  defdelegate filesystem_store(root_dir), to: FilesystemStore
  defdelegate logging_store(store), to: LoggingStore
  defdelegate log_store(log_filepath), to: LogStore
  defdelegate map_store(), to: MapStore
  defdelegate map_store(map), to: MapStore
  defdelegate mapping_store(store, opts), to: MappingStore
  defdelegate pass_through(store), to: PassThrough
  defdelegate persistent_store(log_filepath), to: PersistentStore
  defdelegate switching_store(path_store_map), to: SwitchingStore
end
```

## Next steps

OK, we've got some disk stores now that will let us persist data, keeping it safe between restarts.

Now that we've got local persistent stores, why not go the complete opposite direction and build some remote stores? That is, let's build some stores which compute their storage call results by sending network requests to other servers.

We'll be doing that in the next post in this series.

You can see the latest version of Matryoshka at [my GitHub](https://github.com/julianferrone/matryoshka).