---
date: '2025-06-27T16:24:56+10:00'
draft: true
title: 'Line Oriented Hyperdocuments in Haskell'
tags:
- programming
- haskell
- hypertext
publishDate: '2025-07-27'
series: subtext
summary: "A parser and HTML renderer for Gordon Brander's Subtext, written in Haskell."
---

I had a fascinating [conversation with @johnnulls](https://x.com/johnnulls/status/1931397812740301156) recently about adding

 by creating a extending Subtext:

- Adding metadata to 

But I'm gett

## What is Subtext?

I'll let Gordon [explain](https://github.com/subconsciousnetwork/subtext):

> Subtext is a text-based, line-oriented hypertext format, designed for note-taking.
>
> Subtext markup is made up of ordinary lines of text, which are interpreted as a list of blocks. Lines that are prefixed with magic "sigil" characters are treated as special blocks. Lines without sigils are treated as text blocks. Empty lines are ignored.

This is a really interesting approach! 

I've played around and tried to create new hypertext tools before, but nothing I've written has gripped me, shaken me by the shoulders, and yelled "I'm useful!". 

Most of them have been built around some variant of content-addressable store so that we have [persistent identifiers](https://en.wikipedia.org/wiki/Persistent_identifier) to prevent link rot (typically with addresses provided by hashing the content), but I could never get any of them working in a way that felt ergonomic to write and read. I'll probably return to those ideas in the future.

Subtext is a lot simpler, mostly because it's not the same thing at all. 

Rather than specifying the full system (with stores, servers, and whatnot), Subtext offers a markup language for a linear hypertext format with very easy parsing based off looking for the magic "sigil" characters.

I can try my hand at building a larger system (with Subtext as its core user-interface/input format) later. For now, let's write enough basic functionality to be able to say I've got some sort of minimum viable product for working with Subtext:

1. A **parser**, so that we can parse Subtext documents into a structured datatype (that we can later work with in interesting ways)
2. A **deparser**, so that we can serialize that datatype back into text
3. A **HTML renderer**, since a full native client is going to take a lot of effort and we can whip up a quick and dirty HTML renderer instead, converting a corpus of Subtext documents into a collection of HTML pages we can serve with something like nginx

## Parsing Subtext

I'll take an example snippet of Subtext from [Gordon's guide](https://github.com/subconsciousnetwork/subtext/blob/main/guide.md)):

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

Since Subtext does some sub-line parsing (HTTP URLs, angle-bracket-delimited URLs, slashlinks, and plain text can all occur within the same line), I'll start with an `Inline` type which will store that sub-line data. 

We then construct `Block`s (which correlate to lines of text in a Subtext file) out of `Inline`s for the more complicated kinds of lines, or `Text` for headings (since we don't want to parse for URLs in headings). 

`Blank`s are our blank lines, which we'll keep in the data-type `Block` since we don't want to throw away that info about the source Subtext files when we're parsing, although we'll probably not do anything with them anyway.

I'll also add `Document` as a type syunonym for lists of `Block`s.

If we translate that into code, it gives us a nice core of data types we can import whenever we need to use them:

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


```haskell {linenos=inline linenostart=14 title="/src/Subtextual/Parser.hs"}
------------------------------------------------------------
--                      Text Parsing                      --
------------------------------------------------------------

whitespace :: Parser T.Text
whitespace = takeWhile1 isHorizontalSpace

word :: Parser T.Text
word = takeWhile1 $ not . isSpace

------------------------------------------------------------
--                     Inline Parsing                     --
------------------------------------------------------------

plainText :: Parser Inline
plainText = fmap PlainText $ word <|> whitespace

isUrlChar :: Char -> Bool
isUrlChar c = not $ c == '>' || isSpace c

string' :: String -> Parser T.Text
string' = string . T.pack

bareUrl :: Parser Inline
bareUrl = do
    schema <- string' "https://" <|> string' "http://"
    body <- manyTill anyChar $ lookAhead endOfUrl
    let url = schema <> T.pack body
    return $ BareUrl url

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

isAngledUrlChar :: Char -> Bool
isAngledUrlChar c = not $ c == '<' || c == '>' || isSpace c

angledUrl :: Parser Inline
angledUrl = do
    string' "<"
    url <- takeWhile1 isAngledUrlChar
    string' ">"
    return $ AngledUrl url

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

inline :: Parser Inline
inline = 
    bareUrl
    <|> angledUrl
    <|> slashLink
    <|> plainText

inlines :: Parser [Inline]
inlines = do
    parsed <- many1 inline
    let parsed' = smoosh parsed []
    return parsed'
    where
        smoosh :: [Inline] -> [Inline] -> [Inline]
        smoosh [] finished = reverse finished
        smoosh (PlainText p : todo) (PlainText p' : done) = smoosh todo $ PlainText (p' <> p) : done
        smoosh (i : todo) done = smoosh todo (i : done)

------------------------------------------------------------
--                      Block Parsing                     --
------------------------------------------------------------

----------                 Helpers                ----------

prefixed :: Char -> Parser a -> Parser a
prefixed c parser = char c *> skipSpace *> parser

takeUntilEndOfLine :: Parser T.Text
takeUntilEndOfLine = takeWhile1 $ not . isEndOfLine

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

----------              Blank Blocks              ----------

newLines :: Parser Document
newLines = do
    eols <- many1 (Data.Attoparsec.Text.takeWhile isHorizontalSpace *> endOfLine)
    let count = length eols
    return (replicate (count - 1) Blank)
    <?> "newLines"

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

Now that we can unparse `Inline`s, `Block`s and `Document`s back to the original 

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

------------------------------------------------------------
--                      Block to HTML                     --
------------------------------------------------------------

block :: Block -> Html ()
block (Paragraph p) = (p_ . inlines) p
block (Heading h) = (h2_ . toHtml) h
block (Bullet b) = (li_ . inlines) b
block (Quote q) = (blockquote_ . inlines) q
block Blank = mempty


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
