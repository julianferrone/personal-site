---
date: "2025-05-05T19:44:26+10:00"
draft: true
title: "Matryoshka 3: Persisting Data to Disk"
tags:
- elixir
- programming
publishDate: 2025-06-09
series: matryoshka
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

Then we can implement the Storage methods by reading from files:

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
    ...
```

...and writing to files:

```elixir {linenos=inline linenostart=44 title="/lib/matryoshka/impl/filesystem_store.ex"}
    ...
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

```goat
+------------+-------------+------------+------------+-------+-------------+
| Timestamp  | WRITE       | Key Size   | Value Size | Key   | Value       |
+------------+-------------+------------+------------+-------+-------------+
| 64 bit int | 4 byte atom | 16 bit int | 32 bit int | Binary encoded term |
+------------+-------------+------------+------------+---------------------+

+------------+-------------+------------+---------------------+              
| Timestamp  | DELETE      | Key Size   | Key                 |              
+------------+-------------+------------+---------------------+
| 64 bit int | 4 byte atom | 16 bit int | Binary encoded term |
+------------+-------------+------------+---------------------+               
```

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

We want to minimise how much data the LogStore has to read from disk whenever we retrieve a value. To do so, we'll be storing an in-memory index, which maps references to a combination of file position (where the value starts in the file) and the value size. Whenever we want to retrieve the value for a reference, we'll go to the file position for that reference, then read the number of bytes the index tells us.

As such, whenever we're appending entries to the log file, or reading the log file on a cold start, it'll be very useful to calculate the sizes of:

- The delete entry
- The write entry
- The size of the write entry excluding the value size

```elixir {linenos=inline linenostart=26 title="/lib/matryoshka/impl/log_store/encoding.ex"}
  ...
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

Now that we've defined our log entry formats, let's work on serializing them to disk. We'll start with a function to retrieve the system time, then serialize it to binary:

```elixir {linenos=inline title="/lib/matryoshka/impl/log_store/serialize.ex"}
defmodule Matryoshka.Impl.LogStore.Serialize do
  alias Matryoshka.Impl.LogStore.Encoding
  import :erlang, only: [term_to_binary: 1]

  # ------------------------ Timestamp -----------------------

  def binary_timestamp() do
    timestamp = System.system_time(Encoding.time_unit())
    <<timestamp::big-unsigned-integer-size(Encoding.timestamp_bitsize())>>
  end
  ...
```

Then we'd also like functions for formatting the log entries in binary. These functions will return a tuple in the shape `{entry, entry_size, value_size}` where:

- The entry will be written to disk
- The size of the entry in bytes will be used when calculating our offsets
- The size of the value in bytes will be stored in the index so we know how many bytes to read on retrieving the value from the log file

```elixir {linenos=inline linenostart=11 title="/lib/matryoshka/impl/log_store/serialize.ex"}
  ...
  # ----------------------- Formatting -----------------------

  def format_write_log_entry(key, value) do
    {key_size, key_size_data, key} = pack_key(key)
    {value_size, value_size_data, value} = pack_value(value)

    entry =
      Enum.join([
        Encoding.atom_write_binary(),
        key_size_data,
        value_size_data,
        key,
        value
      ])

    entry_size = Encoding.write_entry_pre_value_size(key_size)

    {prepend_timestamp(entry), entry_size, value_size}
  end

  def format_delete_log_entry(key) do
    {key_size, key_size_data, key} = pack_key(key)

    entry =
      Enum.join([
        Encoding.atom_delete_binary(),
        key_size_data,
        key
      ])

    entry_size = Encoding.delete_entry_size(key_size)

    {prepend_timestamp(entry), entry_size, nil}
  end
  ...
```

These entry formatting functions will need some helper functions. We want to prepend the timestamp to all log entries, which will be done with `prepend_timestamp/1`:

```elixir {linenos=inline linenostart=46 title="/lib/matryoshka/impl/log_store/serialize.ex"}
  ...
  def prepend_timestamp(data) when is_binary(data) do
    timestamp = binary_timestamp()
    timestamp <> data
  end
  ...
```

We also want some helper functions to pack keys and values into an unsigned int, with a size of a given bit width (16 bits for keys, 32 bits for values):

- `size` is the byte size of the binary-encoded term.
- `size_data` is the binary-encoded size using `int_size` bits.
- `binary_term` is the binary representation of the term.

```elixir {linenos=inline linenostart=51 title="/lib/matryoshka/impl/log_store/serialize.ex"}
  ...
  def pack_term(term, int_size) do
    binary = term |> term_to_binary()
    size = byte_size(binary)
    size_data = <<size::big-unsigned-integer-size(int_size)>>
    {size, size_data, binary}
  end

  def pack_key(key), do: pack_term(key, Encoding.key_bitsize())

  def pack_value(value), do: pack_term(value, Encoding.value_bitsize())
  ...
```

Now that we can format log entries, we just need to append entries to the log file:

```elixir {linenos=inline linenostart=62 title="/lib/matryoshka/impl/log_store/serialize.ex"}
  ...
  # ------------------- Writing to Log File ------------------

  def append_write_log_entry(fd, key, value) do
    {entry, relative_offset, value_size} = format_write_log_entry(key, value)
    IO.binwrite(fd, entry)
    {relative_offset, value_size}
  end

  def append_delete_log_entry(fd, key) do
    {entry, relative_offset, value_size} = format_delete_log_entry(key)
    IO.binwrite(fd, entry)
    {relative_offset, value_size}
  end
end
```

We've finished defining how to write log entries, now it's time to define how to read them.

### Deserialize

We'll start with some helper functions with IO:

- `handle_io_result/2` applies a function to the result of IO.binread only if there's no error
- `binread_then_map/3` reads a set number of bytes from a file, then applies the function using `handle_io_result/2`. This is useful for reading bits of data at a time that we can then parse.

```elixir {linenos=inline title="/lib/matryoshka/impl/log_store/deserialize.ex"}
defmodule Matryoshka.Impl.LogStore.Deserialize do
  alias Matryoshka.Impl.LogStore.Encoding
  import :erlang, only: [binary_to_term: 1]

  # __________________ Reading from Log File _________________

  # ----------------------- IO Helpers -----------------------

  def handle_io_result(:eof, _fun), do: :eof
  def handle_io_result({:error, reason}, _fun), do: {:error, reason}
  def handle_io_result(bytes, fun), do: {:ok, fun.(bytes)}

  def binread_then_map(fd, number_bytes, fun) do
    bytes = IO.binread(fd, number_bytes)
    handle_io_result(bytes, fun)
  end
  ...
```

Next, we'll add some functions to read and parse data from the file. These parse the binary into integers, atoms, or timestamps:

```elixir {linenos=inline linenostart=17 title="/lib/matryoshka/impl/log_store/deserialize.ex"}
  ...
  # ------------------ Reading Elixir Types ------------------

  def read_big_unsigned_integer(fd, int_size) do
    number_bytes = Encoding.bits_to_bytes(int_size)

    binread_then_map(fd, number_bytes, fn bytes ->
      <<int::big-unsigned-integer-size(int_size)>> = bytes
      int
    end)
  end

  def read_atom(fd) do
    atom_bytesize = Encoding.atom_bytesize()

    binread_then_map(
      fd,
      atom_bytesize,
      fn bytes ->
        <<binary_atom::binary-size(atom_bytesize)>> = bytes
        atom = binary_to_term(binary_atom)
        atom
      end
    )
  end

  def read_timestamp(fd) do
    timestamp_bitsize = Encoding.timestamp_bitsize()

    with {:ok, timestamp_int} <- read_big_unsigned_integer(fd, timestamp_bitsize) do
      DateTime.from_unix(timestamp_int, Encoding.time_unit())
    else
      other -> other
    end
  end
  ...
```

But the most important part is reading the log entries. When we read a log entry, we consume the timestamp (which currently, we do nothing with) and the entry kind atom, which lets us split the rest of the entry reading to either `read_write_entry/1` (if the atom is `:w`) or `read_delete_entry/1` (if the atom is `:d`). If there's a different atom encoded, we'll return an error value.

```elixir {linenos=inline linenostart=52 title="/lib/matryoshka/impl/log_store/deserialize.ex"}
  ...
  # ------------------- Reading Log Entries ------------------

  # .................... Read Entire Entry ....................

  def read_log_entry(fd) do
    _timestamp = read_timestamp(fd)
    entry_kind = read_atom(fd)
    atom_write = Encoding.atom_write()
    atom_delete = Encoding.atom_delete()

    case entry_kind do
      {:ok, ^atom_write} -> read_write_entry(fd)
      {:ok, ^atom_delete} -> read_delete_entry(fd)
      {:ok, atom} -> {:error, {:no_entry_kind, atom}}
      other -> other
    end
  end
  ...
```

For write entries, we need to read:

1. The key size (16 bit int)
2. The value size (32 bit int)
3. The key, which we parse from binary into a term using `:erlang.binary_to_term/1`
4. The value, which we also parse from binary into a term

Then we return the parsed entry in the form `{:w, key, value}`.

```elixir {linenos=inline linenostart=70 title="/lib/matryoshka/impl/log_store/deserialize.ex"}
  ...
  def read_write_entry(fd) do
    with {:ok, key_size} <- 
           read_big_unsigned_integer(fd, Encoding.key_bitsize()),
         {:ok, value_size} <- 
           read_big_unsigned_integer(fd, Encoding.value_bitsize()),
         {:ok, key} <-
           binread_then_map(fd, key_size, &binary_to_term/1),
         {:ok, value} <-
           binread_then_map(fd, value_size, &binary_to_term/1) do
      {:ok, {Encoding.atom_write(), key, value}}
    else
      error -> error
    end
  end
  ...
```

For delete entries, we only need to read the key size and the key, parse the key into a term, then return the parsed entry in the form `{:d, key}`.

```elixir {linenos=inline linenostart=85 title="/lib/matryoshka/impl/log_store/deserialize.ex"}
  ...
  def read_delete_entry(fd) do
    with {:ok, key_size} <- 
           read_big_unsigned_integer(fd, Encoding.key_bitsize()),
         {:ok, key} <-
           binread_then_map(fd, key_size, &binary_to_term/1) do
      {:ok, {Encoding.atom_delete(), key}}
    else
      error -> error
    end
  end
  ...
```

Now that we can read one entry at a time, we need the ability to read through the entire log file, keeping track of:

- what keys (references) we've seen
- the position in the file that the associated value has
- the size of the associated value

These will eventually become the index in LogStore, a map of type `reference -> {value_position, value_size}`.

We start with a helper function `load_offsets/1` which takes the file descriptor of the log file. `load_offsets/1` starts off loading the offsets with an empty index map and a position of 0 (i.e. because we're at the beginning of the file):

```elixir {linenos=inline linenostart=95 title="/lib/matryoshka/impl/log_store/deserialize.ex"}
  ...
  # ............... Load Offsets and Value Size ..............

  def load_offsets(fd) do
    load_offsets(fd, Map.new(), 0)
  end
  ...
```

OK, so now, how do we read the log file?

Firstly, let's set our position in the file to the current offset that was passed in. We've already read up to this position, so we should continue reading the file from it.

```elixir {linenos=inline linenostart=101 title="/lib/matryoshka/impl/log_store/deserialize.ex"}
  ...
  def load_offsets(fd, offsets, current_offset) do
    :file.position(fd, current_offset)
    ...
```

Afterwards, we'll read the timestamp and entry-type atom, which we'll use to read the key term, key size, and value size from the entry (more on this later):

```elixir {linenos=inline linenostart=104 title="/lib/matryoshka/impl/log_store/deserialize.ex"}
    ...
    with {:ok, _timestamp} <- read_timestamp(fd),
         {:ok, entry_kind} <- read_atom(fd),
         {:ok, {key, key_size, value_size}} <-
           load_offsets_entry(fd, entry_kind) do
      ...
```

Now, it's time to calculate the various sizes we need. We'll need two sizes:

1. `relative_offset_to_value`, which is calculated as the size of the log entry from the beginning of the timestamp to the beginning of the value
    - Delete entries have no value, so this is calculated as the whole delete entry size
2. `relative_offset_to_end`, which is calculated as the size of the whole log entry (including the value for write entries)

```elixir {linenos=inline linenostart=109 title="/lib/matryoshka/impl/log_store/deserialize.ex"}
      ...
      relative_offset_to_value =
        case value_size do
          nil ->
            Encoding.delete_entry_size(key_size)

          _nonzero ->
            Encoding.write_entry_pre_value_size(key_size)
        end

      relative_offset_to_end =
        case value_size do
          nil -> Encoding.delete_entry_size(key_size)
          value_size -> Encoding.write_entry_size(key_size, value_size)
        end
      ...
```

Now we need to calculate the `value_offset`, which is the absolute position of the value in the log file. This is equal to the current offset (which is set to the starting position of the timestamp). We then put the information into the offsets map, which we'll later use as the index in LogStore:

```elixir {linenos=inline linenostart=124 title="/lib/matryoshka/impl/log_store/deserialize.ex"}
      ...
      value_offset =
        current_offset + relative_offset_to_value

      offsets = Map.put(offsets, key, {value_offset, value_size})
      ...
```

And then we need to calculate the position of the end of the entry, so that we can continue the recursion with the current offset set to the start of the next entry. We continue until we reach an `:eof` which informs us that we've traversed the entire log file, and therefore we can return the offsets:

```elixir {linenos=inline linenostart=129 title="/lib/matryoshka/impl/log_store/deserialize.ex"}
      ...
      absolute_offset =
        current_offset + relative_offset_to_end

      load_offsets(fd, offsets, absolute_offset)
    else
      :eof -> offsets
    end
  end
  ...
```

OK so that's all well and good, but we still need to fill in the loading of the offset entries. `load_offsets_entry/2` delegates the offset loading to either `load_offsets_write_entry/1` or `load_offsets_write_entry/1` depending on the entry-type atom:

```elixir {linenos=inline linenostart=138 title="/lib/matryoshka/impl/log_store/deserialize.ex"}
  ...
  def load_offsets_entry(fd, entry_kind) do
    atom_write = Encoding.atom_write()
    atom_delete = Encoding.atom_delete()

    case entry_kind do
      ^atom_write -> load_offsets_write_entry(fd)
      ^atom_delete -> load_offsets_delete_entry(fd)
      atom when is_atom(atom) -> {:error, {:no_lin_kind, atom}}
      other -> other
    end
  end
  ...
```

To load the offsets for a write entry, we read:

1. The key size
2. The value size
3. The key, which we parse from binary into a term

Then return this info in a tuple `{key, key_size, value_size}`:

```elixir {linenos=inline linenostart=150 title="/lib/matryoshka/impl/log_store/deserialize.ex"}
  ...
  def load_offsets_write_entry(fd) do
    with {:ok, key_size} <- 
           read_big_unsigned_integer(fd, Encoding.key_bitsize()),
         {:ok, value_size} <- 
           read_big_unsigned_integer(fd, Encoding.value_bitsize()) do
      binread_then_map(fd, key_size, fn key_bin ->
        key = binary_to_term(key_bin)
        {key, key_size, value_size}
      end)
    end
  end
  ...
```

To load the offsets for a delete entry, we read:

1. The key size
2. The key, which we parse from binary into a term

And we also set the value size to `nil` since there's no value in a delete entry. We'll take advantage of this when retrieving values in LogStore, as we'll know that we can return an error for `fetch/2` or `nil` for `get/2`.

`load_offsets_delete_entry/1` then returns this info in a tuple `{key, key_size, nil}`:

```elixir {linenos=inline linenostart=160 title="/lib/matryoshka/impl/log_store/deserialize.ex"}
  ...
  def load_offsets_delete_entry(fd) do
    key_size = read_big_unsigned_integer(fd, Encoding.key_bitsize())

    binread_then_map(fd, key_size, fn key_bin ->
      key = binary_to_term(key_bin)
      {key, key_size, nil}
    end)
  end
  ...
```

That concludes indexing the log file. We just need to add a function to use the value offset and value size data to read values from the log file, which we do in `get_value/3`:

1. We read `size` bytes from the position `offset` using the Erlang function `:file.pread/3`
2. Then, if the bytes are successfully read, we convert the bytes back into a term and return it
3. Otherwise, it returns the error (`:eof` or `{:error, reason}`)

```elixir {linenos=inline linenostart=169 title="/lib/matryoshka/impl/log_store/deserialize.ex"}
  ...
  # ----------------- Read Value at Position -----------------

  def get_value(fd, offset, size) when not is_nil(size) do
    with {:ok, bin} <- :file.pread(fd, offset, size) do
      {:ok, binary_to_term(bin)}
    else
      other -> other
    end
  end
end
```

### Combining into LogStore

Finally it's time to wrap up all this functionality into the LogStore.

The LogStore will need 3 components:

1. A reader, which will read values from the log file on `fetch/2` and `get/2`
2. A writer, which will append new entries to the log file on `put/3` and `delete/2`
3. An index, which will store the mapping `reference -> {value position, value size}` we receive from `Deserialize.load_offsets/1`.

We start, as always, with a struct and a constructor function.

We do want to be careful with `log_store/1`, however. If the log file already exists, we want to load the offsets into the LogStore index beforehand, and open the writer in append mode (so that all writes go to the end of the file). If the log file doesn't already exist, we'll open the writer in write mode (which creates an empty log file) before recreating the reader and initializing an empty index map.

```elixir {linenos=inline title="/lib/matryoshka/impl/log_store/log_store.ex"}
defmodule Matryoshka.Impl.LogStore do
  alias Matryoshka.Impl.LogStore.Deserialize
  alias Matryoshka.Impl.LogStore.Serialize
  alias Matryoshka.Storage

  @enforce_keys [:reader, :writer, :index]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          reader: File.io_device(),
          writer: File.io_device(),
          index: map()
        }

  def log_store(log_filepath) do
    {reader, writer, index} =
      case File.open(log_filepath, [:binary, :read]) do
        {:ok, reader} ->
          index = Deserialize.load_offsets(reader)
          {:ok, writer} = File.open(log_filepath, [:binary, :append])
          {reader, writer, index}

        {:error, _reason} ->
          {:ok, writer} = File.open(log_filepath, [:binary, :write])
          {:ok, reader} = File.open(log_filepath, [:binary, :read])
          index = Map.new()
          {reader, writer, index}
      end

    %__MODULE__{reader: reader, writer: writer, index: index}
  end
  ...
```

Now, let's implement the storage protocols. `fetch/2` is the most complicated as we want to return a reason when there's an error retrieving the value. We tag both steps in the `with` macro with an initial (`:index` or `:store`) so that we can pattern match on the individual steps and reformat the errors into our `{:error, reason}` format. On line **48** you can see that if the index map returns a `nil`, we know that the value has been deleted, so we return a "No reference" error.

```elixir {linenos=inline linenostart=32 hl_lines=[17] title="/lib/matryoshka/impl/log_store/log_store.ex"}
  ...
  defimpl Storage do
    def fetch(store, ref) do
      value =
        with {:index, {:ok, {offset, size}}} when not is_nil(size) <-
               {:index, Map.fetch(store.index, ref)},
             {:store, {:ok, value}} <-
               {:store,
                Deserialize.get_value(
                  store.reader,
                  offset,
                  size
                )} do
          value
        else
          {:index, :error} -> {:error, {:no_ref, ref}}
          {:index, {:ok, {_position, nil}}} -> {:error, {:no_ref, ref}}
          {:store, {:error, reason}} -> {:error, reason}
          {:store, :eof} -> {:error, :eof}
        end

      {store, value}
    end
    ...
```

`get/2` on the other hand is much easier, as we simply return `nil` on any error:

- if the value size is nil
- if the value info isn't found in the index (from `Map.fetch(store.index, ref)`)
- if the value isn't found in the log file

```elixir {linenos=inline linenostart=55 title="/lib/matryoshka/impl/log_store/log_store.ex"}
    ...
    def get(store, ref) do
      value =
        with {:ok, {offset, size}} when not is_nil(size) <-
               Map.fetch(store.index, ref),
             {:ok, value} <-
               Deserialize.get_value(store.reader, offset, size) do
          value
        else
          _ -> nil
        end

      {store, value}
    end
    ...
```

`put/2` and `delete/2` are even more simple. We append an entry (write for puts, delete for deletes) to the log file, update the index with the value information, then return the updated LogStore:

```elixir {linenos=inline linenostart=69 title="/lib/matryoshka/impl/log_store/log_store.ex"}
    ...
    def put(store, ref, value) do
      {position, size} = Serialize.append_write_log_entry(store.writer, ref, value)
      index = Map.put(store.index, ref, {position, size})
      %{store | index: index}
    end

    def delete(store, ref) do
      {position, size} = Serialize.append_delete_log_entry(store.writer, ref)
      index = Map.put(store.index, ref, {position, size})
      %{store | index: index}
    end
  end
end
```

And with that, we have finally finished writing the append-only-log-backed store LogStore.

### Packaging into PersistentStore

We'll cap off this post with an example of a new store we construct with LogStore: a persistent store that uses LogStore as a disk-based source of truth, with caching functionality backed by an in-memory MapStore.

```elixir {linenos=inline title="/lib/matryoshka/impl/persistent_store.ex"}
defmodule Matryoshka.Impl.PersistentStore do
  alias Matryoshka.Impl.CachingStore
  alias Matryoshka.Impl.LogStore

  def persistent_store(log_filepath) do
    LogStore.log_store(log_filepath)
    |> CachingStore.caching_store()
  end
end
```

It's as easy as that. Whenever we want to compose stores and store combinators together, we can just pass them into each-other using good old fashioned function calls and the Elixir pipeline operator.

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