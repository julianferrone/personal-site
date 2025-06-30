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

This is a really interesting approach! I've played around and tried to create new hypertext tools (with new formats), but nothing I've written has gripped me, shaken me by the shoulders, and yelled "I'm useful!". Most of them have been built around some variant of content-addressable store so that we have [persistent identifiers](https://en.wikipedia.org/wiki/Persistent_identifier) to prevent link rot (typically with addresses provided by hashing the content), but I could never get any of them working in a way that felt ergonomic to write and read. I'll probably return to those ideas in the future.

Subtext is a lot simpler, mostly because it's not the same thing at all. Rather than building out the full system (with stores, servers, and whatnot), Subtext just offers a markup language for a linear hypertext format with very easy parsing based off looking for the magic "sigil" characters.



## Parsing Subtext

I'll use [attoparsec](https://hackage.haskell.org/package/attoparsec) to build the parsing libraries 

## Unparsing Subtext

## Rendering Subtext to HTML

## Next steps
