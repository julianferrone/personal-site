---
date: '2025-06-27T16:34:42+10:00'
draft: true
title: 'Adding Tags, Triples, and Transclusion to Subtextual'
tags:
- programming
- haskell
- hypertext
publishDate: '2025-07-27'
series: subtext
summary: "I extend Gordon Brander's Subtext with markup for metadata (tags, key-value pairs, and triples) and transclusion."
---

Gordon Brander's Subtext is a pretty simple markup format for hyperdocuments. There's no  presentation markup (i.e., something like CSS.) There's no programming markup to add reactivity (like JavaScript.) There's only a focused markup for semantics that includes:

- 4 kinds of sub-line-level semantic chunks (the `Inline` data type in Subtextual):
    1. Plain text
    2. Bare URLs (which Subtextual currently only recognises if they use the HTTP(S) scheme) to external resources
    3. Angle-bracket-delimited URLs to external resources
    4. Slashlinks to other Subtext documents
- 5 kinds of line-level semantic chunks (the `Block` data type in Subtextual):
    1. Regular paragraphs
    2. Headings
    3. Bullet points
    4. Quotes
    5. Blank lines

So of course, I decided to ruin that beautiful simplicity by adding a bunch more specifications for line-level semantics—as well as adding parsing for those primitives in my Haskell library [Subtextual](https://github.com/julianferrone/subtextual)—to encode metadata and transclusion.

## Adding metadata

Subtext documents are predominantly human-readable. By adding metadata, we can encode semantic information that's easier for [GOFAI](https://en.wikipedia.org/wiki/GOFAI)/symbolic AI) to process, much like the RDF format. 

There's three kinds of metadata:

1. Tags
2. Key-value pairs
3. Triples

These are unary, binary, and ternary `Block` constructors, respectively.

Each provides progressively more power in terms of what they can express.

A tag attaches a blob of data to a Subtext document, i.e. which we can interpret as the category that the document belongs to.

Key-value pairs attach a name to that blob of data (which is the value in the pair).

Triples enhance key-value pairs by adding a 



### Tags

Tags let us sort documents into categories.

Syntactically, tag blocks start with `!`, followed by some arbitrary amount of whitespace, and then any number of non-whitespace characters (e.g. alphanumerics, dashes, underscores, slashes).

For example, the tags for this article would be encoded like:

```subtext-extended
! haskell
! hypertext
! programming
```

### Key-value pairs

### Triples

## Adding transclusion

Transclusion explanation