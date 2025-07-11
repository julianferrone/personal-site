---
date: '2025-06-27T16:24:56+10:00'
draft: false
title: 'Line Oriented Hyperdocuments in Haskell'
tags:
- programming
- haskell
- hypertext
publishDate: '2025-07-01'
series: subtext
summary: "A parser and HTML renderer for Gordon Brander's Subtext, written in Haskell."
aliases: 
- "posts/line-oriented-hyperdocs/"
---

I had a fascinating [conversation with @johnnulls](https://x.com/johnnulls/status/1931409337936969892) recently about creating a lightweight markup format for hyperdocuments, extending Subtext to add various affordances for metadata like key-value pairs and tags.

But I'm getting ahead of myself.

## What is Subtext?

I'll let Gordon [explain](https://github.com/subconsciousnetwork/subtext):

> Subtext is a text-based, line-oriented hypertext format, designed for note-taking.
>
> Subtext markup is made up of ordinary lines of text, which are interpreted as a list of blocks. Lines that are prefixed with magic "sigil" characters are treated as special blocks. Lines without sigils are treated as text blocks. Empty lines are ignored.

This is a really interesting approach! 

I've played around and tried to create new hypertext tools before, but nothing I've written has really gripped me.

Most of them have been built around some variant of content-addressable store so that we have [persistent identifiers](https://en.wikipedia.org/wiki/Persistent_identifier) to prevent link rot (typically with addresses provided by hashing the content), but I could never get any of them working in a way that felt ergonomic to write and read. I think I'll return to those ideas one day.

Subtext is a lot simpler, mostly because it's not the same thing at all. 

Rather than specifying the full system (with stores, servers, and whatnot), Subtext offers a markup language for a linear hypertext format with very easy parsing based off looking for the magic "sigil" characters.

Let's write enough basic functionality to be able to say I've got some sort of minimum viable product for working with Subtext:

1. A **parser**, so that we can parse Subtext documents into a structured datatype (that we can later work with in interesting ways)
2. An **unparser**, so that we can serialize that datatype back into text
3. A **HTML renderer**, since a full native client is going to take a lot of effort and we can whip up a quick and dirty HTML renderer instead, converting a corpus of Subtext documents into a collection of HTML pages we can serve with something like nginx

I'll call this Haskell library **Subtextual**.

## Parsing Subtext

I'll take an example snippet of Subtext from [Gordon's guide](https://github.com/subconsciousnetwork/subtext/blob/main/guide.md):

```subtext
# Heading

Plain text.

- List item
- List item

> Quoted text

URLs like https://example.com are automatically linked.

You can also link to local pages using short /slashlinks.
```

I'll use [attoparsec](https://hackage.haskell.org/package/attoparsec) to build the parsing functionality, since I've used it before and I enjoy working with it.

Since Subtext does some sub-line parsing, I'll start with an `Inline` type which will store that sub-line data. There's 4 different kinds of text we can see in a line:

1. Plain text
2. Bare URLs, recognised by the prefix "http://" or "https://"
3. URLs delimited by angle-brackets
4. Slashlinks

We then construct `Block`s (which correlate to lines of text in a Subtext file) out of `Inline`s for the more complicated kinds of lines, or `Text` for headings (since we don't want to parse for URLs in headings). 

`Blank`s are our blank lines, which we'll keep in the data-type `Block` since we don't want to throw away that info about the source Subtext files when we're parsing, although we'll probably not do anything with them anyway.

I'll also add `Document` as a type synonym for lists of `Block`s.

That gives us a nice core of data types:

```haskell {linenos=inline title="/src/Subtextual/Core.hs"}
module Subtextual.Core
    ( Inline(..), Block(..), Document
    ) where

import qualified Data.Text as T

data Inline =
    PlainText T.Text
    | BareUrl T.Text
    | AngledUrl T.Text
    | SlashLink T.Text
    deriving (Show, Eq)

data Block = 
    Paragraph [Inline]
    | Heading T.Text
    | Bullet [Inline]
    | Quote [Inline]
    | Blank
    deriving (Show, Eq)

type Document = [Block]
```

Now we're ready to parse some text. We'll start with some imports:

```haskell {linenos=inline title="/src/Subtextual/Parser.hs"}
module Subtextual.Parser
    (nonBlankBlock, document) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Text as T

import Subtextual.Core
```

With that out of the way, we can work on parsing. 

Attoparsec is a parser combinator library, which means we build up complex parsers out of simple ones. So let's start with those building blocks.

Since we want to start looking for URLs and slashlinks at word boundaries, we'll need to parse in whitespace and non-whitespace characters separately. Every time we see a new word, we'll first try to parse it with our more structured parsers for bare URLs, angle-delimited URLs, and slashlinks. If those fail, we'll fall back to parsing them as plain text.

So we write the two text parsers:

1. `whitespace` to parse spaces and tabs
2. `word` to parse non-space characters

```haskell {linenos=inline linenostart=14 title="/src/Subtextual/Parser.hs"}
------------------------------------------------------------
--                      Text Parsing                      --
------------------------------------------------------------

whitespace :: Parser T.Text
whitespace = takeWhile1 isHorizontalSpace <?> "whitespace"

word :: Parser T.Text
word = takeWhile1 $ not . isSpace <?> "word"
```

After that we'll want to work on parsing `Inline`s.

`PlainText` should get parsed whenever we have whitespace, or a string of characters that didn't get parsed as a URL or slash-link:

```haskell {linenos=inline linenostart=24 title="/src/Subtextual/Parser.hs"}
------------------------------------------------------------
--                     Inline Parsing                     --
------------------------------------------------------------

plainText :: Parser Inline
plainText = PlainText <$> (word <|> whitespace) <?> "plainText"
```

`BareUrl` was an interesting challenge. According to the [spec](https://github.com/subconsciousnetwork/subtext/blob/main/specification.md):

> Subtext parsers MUST implement automatic linking for certain URLs that are not in brackets.

We could just keep parsing characters into a `BareUrl` until we hit whitespace, but there's a problem with that approach---we want to be able to intelligently ignore punctuation like periods, semicolons, or commas at the end of URLs.

If we parse in e.g. `Here's a link: https://google.com.`, we want the parsed link to be "https://google.com", with the final period being parsed as plaintext.

To solve this issue, we do lookahead on the next bit of the input to see if we've reached the end of the URL, which we define as either:

1. A **punctuation boundary** (a period, semicolon, or comma followed by either whitespace characters or the end of the line)
2. A **space**, which includes spaces, tabs, and newlines
3. Or the **end of input**

```haskell {linenos=inline linenostart=31 title="/src/Subtextual/Parser.hs"}
string' :: String -> Parser T.Text
string' = string . T.pack

bareUrl :: Parser Inline
bareUrl = do
    schema <- string' "https://" <|> string' "http://"
    body <- manyTill anyChar $ lookAhead endOfUrl
    let url = schema <> T.pack body
    return $ BareUrl url
    <?> "bareUrl"

    where
        endOfUrl :: Parser ()
        endOfUrl = 
            punctuationBoundary 
            <|> space $> () 
            <|> endOfInput

        punctuationBoundary :: Parser ()
        punctuationBoundary = do
            c1 <- char '.' <|> char ';' <|> char ','
            c2 <- skip isSpace <|> endOfLine
            return ()
```

Parsing `AngledUrl`s is easier, since all we have to do is look for the [angle-bracket-delimited text](https://github.com/subconsciousnetwork/subtext/blob/main/specification.md).

> URLs are wrapped in angle brackets, and can appear anywhere within a text, link, or quote block

```haskell {linenos=inline linenostart=55 title="/src/Subtextual/Parser.hs"}
isAngledUrlChar :: Char -> Bool
isAngledUrlChar c = not $ c == '<' || c == '>' || isSpace c

angledUrl :: Parser Inline
angledUrl = do
    string' "<"
    url <- takeWhile1 isAngledUrlChar
    string' ">"
    return $ AngledUrl url
    <?> "angledUrl"
```

Slashlinks are also easy since we don't need lookahead---the [spec](https://github.com/subconsciousnetwork/subtext/blob/main/specification.md) tells us that:

> Generally, a slashlink is a / followed by any number of alphanumeric characters, dashes -, underscores _.

And so parsing a slashlink is as easy as looking for the initial forward-slash `/` character and then parsing in any alphanumeric characters, dashes, and slashes.

```haskell {linenos=inline linenostart=66 title="/src/Subtextual/Parser.hs"}
isSlashLinkChar :: Char -> Bool
isSlashLinkChar c = 
    isAlpha c 
    || isDigit c 
    || c == '-' 
    || c == '_' 
    || c == '/' 

slashLink :: Parser Inline
slashLink = do
    char '/'
    link <- takeWhile1 isSlashLinkChar
    return $ SlashLink link
    <?> "slashLink"
```

Now that we've defined each of the individual `Inline` parsers, we stitch them together into one big `Inline` parser using the Alternative operator `<|>`, which tries each parser in order (backtracking on failure) until it returns the first successful parse.

There's only one gotcha here, which is that `plainText` needs to be the last parser in the list, since it's indiscriminate about what text it'll parse and it'll happily consume the rest of the input until the end of the line. By being careful about the order, we give our more structured (and more discriminating) parsers---`bareUrl`, `angledUrl`, and `slashLink`---the first crack at parsing the text.

```haskell {linenos=inline linenostart=81 title="/src/Subtextual/Parser.hs"}
inline :: Parser Inline
inline = 
    bareUrl
    <|> angledUrl
    <|> slashLink
    <|> plainText
    <?> "inline"
```

Of course, our `Block`s don't consume `Inline`s, they consume `[Inline]`s, so we need a parser for those too.

Now, we could just lift the parser using `many1`, which takes some parser and runs it at least once (returning a list of values), but there's something that nags at me with that approach---since we're parsing whitespace and non-space characters into separate elements, every single space between words would have its own `PlainText` entry in the `Block`, which seems excessive.

To fix that, I decided to do some post-processing over the list to `smoosh` together contiguous `PlainText`s into one large `PlainText` by concatenating their text:

```haskell {linenos=inline linenostart=89 title="/src/Subtextual/Parser.hs"}
inlines :: Parser [Inline]
inlines = do
    parsed <- many1 inline
    let parsed' = smoosh parsed []
    return parsed'
    <?> "inlines"
    where
        smoosh :: [Inline] -> [Inline] -> [Inline]
        smoosh [] finished = reverse finished
        smoosh (PlainText p : todo) (PlainText p' : done) = 
            smoosh todo $ PlainText (p' <> p) : done
        smoosh (i : todo) done = smoosh todo (i : done)
```

With `[Inline]` parsing out of the way, it's time to start parsing `Block`s.

Since Subtext is based off checking magic sigil characters at the start of the line, I created a helper combinator `prefixed` which looks for a given character, skips any spaces, then runs a parser.

The `Block` parsers are fairly self-explanatory. Most of them look for some character at the start of the line before parsing in either a list `[Inline]` or plain `Text` in the case of `Heading` since we don't want to search for slashlinks and other URLs in section headers.

The only exception to the "look for a magic sigil char" rule is `paragraph`, which like `plainText` will happily accept any input up until the end of the line, and that means that we again need to be careful about our ordering to leave `paragraph` as the final subparser of `nonBlankBlock`.

```haskell {linenos=inline linenostart=102 title="/src/Subtextual/Parser.hs"}
------------------------------------------------------------
--                      Block Parsing                     --
------------------------------------------------------------

----------                 Helpers                ----------

prefixed :: Char -> Parser a -> Parser a
prefixed c parser = char c *> skipSpace *> parser

takeUntilEndOfLine :: Parser T.Text
takeUntilEndOfLine = takeWhile1 $ not . isEndOfLine <?> "takeUntilEndOfLine"

----------            Non-Blank Blocks            ----------

paragraph :: Parser Block
paragraph = Paragraph <$> inlines <?> "paragraph"

heading :: Parser Block
heading = Heading <$> prefixed '#' takeUntilEndOfLine <?> "heading"

bullet :: Parser Block
bullet = Bullet <$> prefixed '-' inlines <?> "bullet"

quote :: Parser Block
quote = Quote <$> prefixed '>' inlines <?> "quote"

nonBlankBlock :: Parser Block
nonBlankBlock = 
    heading
    <|> bullet
    <|> quote
    <|> paragraph
    <?> "nonBlankBlock"

nonBlankBlocks :: Parser Document
nonBlankBlocks = many1 nonBlankBlock <?> "nonBlankBlocks"
```

Ahh, but we're not yet parsing blank lines. Why not just add some `blank` parser to `block` (a hypothetical parser to parse any `Block`) that'll look for a newline and spits out a `Blank` element?

Basically, it's a counting problem. If we parse newlines as `blank`s but have our ultimate `document :: Parser Document` consist of looking for multiple pairs of parsed text followed by newlines, we'll get the number of `Blank`s wrong---we'd have to see 3 newline characters before we emitted one `Blank`.

Imagine we have some line like `Line 1\n\nLine 2` that we want to parse with `document`.

1. `Line1` would be parsed in as plain text
2. The first `\n` would be parsed in as the end of the first line,
3. The second `\n` would be parsed in as a `Blank`

But now the parser would be expecting a third `\n` to be the end of the second (blank) line, and it's never going to find it, which will cause the whole thing to stop accepting input, causing problems

The fix is to instead count newlines and then emit a list of `Blank`s which is 1 item shorter than the number of newlines, which gets the behaviour we want:

- `\n` gets parsed as an empty list `[]`
- `\n\n` gets parsed as one blank line `[Blank]`
- `\n\n\n` gets parsed as two blank lines `[Blank, Blank]`, etc.

```haskell {linenos=inline linenostart=139 title="/src/Subtextual/Parser.hs"}
----------              Blank Blocks              ----------

newLines :: Parser Document
newLines = do
    eols <- many1 (Data.Attoparsec.Text.takeWhile isHorizontalSpace *> endOfLine)
    let count = length eols
    return $ replicate (count - 1) Blank
    <?> "newLines"
```

This is also why we wrote the parser `nonBlankBlocks`---with two parsers `Parser Document` that parse lists of blocks, we can combine the two parsers into a single `document` parser.

After we figure that complication out, parsing a whole `Document` is pretty easy---we parse many newlines and non-blank blocks at a time, then concatenate them together from a `[Document]` into a `Document`:

```haskell {linenos=inline linenostart=148 title="/src/Subtextual/Parser.hs"}
------------------------------------------------------------
--                    Document Parsing                    --
------------------------------------------------------------

document :: Parser Document
document = concat <$> many1 (nonBlankBlocks <|> newLines) <?> "document"
```

## Unparsing Subtext

In comparison to parsing, unparsing is unbelievably easy. 

To unparse `Inline` and `Block`, all we need to do is pattern match and provide the expected text formatting.

To unparse a `Document`, we map `block` over our list of `Block`s to generate the text snippets, then concatenate them with newlines in-between each snippet.

```haskell {linenos=inline title="/src/Subtextual/Unparser.hs"}
module Subtextual.Unparser
    (inline, block, document) where

import Subtextual.Core
import qualified Data.Text as T

inline :: Inline -> T.Text
inline (PlainText p) = p
inline (BareUrl url) = url
inline (AngledUrl url) = T.pack "<" <> url <> T.pack ">"
inline (SlashLink sl) = T.pack "/" <> sl

inlines :: [Inline] -> T.Text
inlines = mconcat . map inline

block :: Block -> T.Text
block (Paragraph p) = inlines p
block (Heading h) = T.pack "# " <> h
block (Bullet b) = T.pack "- " <> inlines b
block (Quote q) = T.pack "> " <> inlines q
block Blank = T.pack ""

document :: Document -> T.Text
document = T.intercalate (T.pack "\n") . map block
```

## Rendering Subtext to HTML

Now that we can unparse `Inline`s, `Block`s and `Document`s back to the original text file, let's work on rendering Subtext to HTML so that we can serve a bundle of static HTML pages over the web. 

I'll use **Lucid** as the HTML templating DSL for this, since it's a simple library that's easy to read. I'm not looking to add unnecessary complexity just to output HTML.

Converting an `Inline` is as simple as pattern-matching on each data constructor, while converting the list `[Inline]` (as wrapped by our `Block`s) only requires concatenating the HTML fragments together:

```haskell {linenos=inline title="/src/Subtextual/Html.hs"}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module Subtextual.Html (block, document) where

import Subtextual.Core
import Lucid

------------------------------------------------------------
--                     Inlines to HTML                    --
------------------------------------------------------------

inline :: Inline -> Html ()
inline (PlainText p) = (span_ . toHtml) p
inline (BareUrl url) = a_ [href_ url] $ toHtml url
inline (AngledUrl url) = a_ [href_ url] $ toHtml url
inline (SlashLink sl) = a_ [href_ sl, class_ "slashlink"] $ toHtml sl

inlines :: [Inline] -> Html ()
inlines = mconcat . map inline
```

Outputting HTML for `Block`s is also just a pattern-match away:

```haskell {linenos=inline linenostart=21 title="/src/Subtextual/Html.hs"}
------------------------------------------------------------
--                      Block to HTML                     --
------------------------------------------------------------

block :: Block -> Html ()
block (Paragraph p) = (p_ . inlines) p
block (Heading h) = (h2_ . toHtml) h
block (Bullet b) = (li_ . inlines) b
block (Quote q) = (blockquote_ . inlines) q
block Blank = mempty
```

Since `Document` is really just a `[Block]`, surely we can just `map block` over a `[Block]` to get a list of HTML fragments that we concatenate together, right?

Unfortunately, it's not so easy.

HTML requires that we wrap our lists of `Bullet`s into a list element---either ordered `ol` or unordered `ul`, while if we were to just `map block`, we'd have list items adjacent to paragraphs, like:

<p>This is a plain text paragraph.</p><li>This is a bullet.</li>`.

To achieve this, I'll first pre-process the `Document` to group up adjacent `Bullet`s into a single list, then wrap those lists with a `ul` element when we output HTML:

```haskell {linenos=inline linenostart=32 title="/src/Subtextual/Html.hs"}
------------------------------------------------------------
--                    Document to HTML                    --
------------------------------------------------------------

data Group a =
    Single a
    | Bullets [a]

document :: Document -> Html ()
document = mconcat . map groupHtml . group' where
    groupHtml :: Group Block -> Html ()
    groupHtml (Single b) = block b
    groupHtml (Bullets bs) = ul_ $ (mconcat . map block) bs

    group' :: Document -> [Group Block]
    group' doc = group doc []

    group :: Document -> [Group Block] -> [Group Block]
    group [] done = (reverse . map reverseGroup) done
    group (Bullet b : todo) (Bullets bs : done) = group todo $ Bullets (Bullet b : bs) : done
    group (Bullet b : todo) done = group todo $ Bullets [Bullet b] : done
    group (b : todo) done = group todo $ Single b : done

    reverseGroup :: Group a -> Group a
    reverseGroup (Single s) = Single s
    reverseGroup (Bullets bs) = Bullets $ reverse bs
```

## Next steps

That wraps up my alpha version of Subtextual!

All the above functionality is also supported by a small battery of unit tests, and you can see the [latest version of Subtextual at my GitHub](https://github.com/julianferrone/subtextual).