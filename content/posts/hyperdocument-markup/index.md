TODO: introduction, Gordon Brandon's Subtext

TODO: XKCD "1 more standard"

Here's some example text of the lines:
```
text
> quote block
! tag
! key value
: subject predicate object
/link
```

```haskell
data Block =
  Text String
  | Quote String
  | Tag String
  | KeyValue String String
  | Triple String String String
  | Link String
```

TODO: Maybe we want links to have a schema? Like `/file/test.txt`

TODO: monadic parser for a line, using nom

TODO: parser for whole file

TODO: renderer

NB: don't think we need an editor, editors are very complicated. Instead we can let users use their own editor, just have a renderer. Maybe just output HTML? But then what if we want to make it reactive?

Maybe:

- phase 1: output HTML
- phase 2: reactive renderer (as changes are made to file)