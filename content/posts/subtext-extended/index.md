---
date: '2025-06-27T16:34:42+10:00'
draft: true
title: 'Adding Tags, Triples, and Transclusion to Subtextual'
tags:
- programming
- haskell
- hypertext
publishDate: '2025-08-10'
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

So of course, I decided to ruin that beautiful simplicity by adding a bunch more specifications for line-level semantics---as well as adding parsing for those primitives in my Haskell library [Subtextual](https://github.com/julianferrone/subtextual)---to encode metadata and transclusion.

## Adding metadata

Why add metadata to Subtext?

It's because Subtext documents are designed to be human-readable---and *only* human-readable. 

There's no affordances for machines to read them.

Metadata fixes that by encoding semantic information that symbolic AI (aka [GOFAI](https://en.wikipedia.org/wiki/GOFAI)) can process.

Sure, you could query them with things like LLMs or search across a collection of them using something like a [term frequency-inverse document frequency search](https://en.wikipedia.org/wiki/Tf–idf) (since they're just text documents at the end of the day), but I'd prefer to add something that a proper reasoner (say, written in Prolog) could work with.

I chose to add three kinds of metadata:

1. Tags
2. Key-value pairs
3. Triples

These are unary, binary, and ternary `Block` constructors, respectively.

Each provides progressively more power in terms of what they can express.

A tag attaches a **keyword** to a Subtext document, which we can interpret as a category that the document belongs to.

Key-value pairs attach a name (the **key**) to a blob of data (the **value**).

Triples enhance key-value pairs by adding a relationship (the **predicate**) between the key (now called the **subject**) and the value (aka the **object**).

```goat
                          +-----------+
                          | Keyword   | Tag
+-----------+             +-----------+
| Key       |             | Value     | Key-value pairs
+-----------+-------------+-----------+
| Subject   | Predicate   | Object    | Triples
+-----------+-------------+-----------+
```

### Tags

A **tag** assigns a keyword to documents.

Syntactically, tag blocks start with `!`, followed by some arbitrary amount of whitespace, and then any number of non-whitespace characters (e.g. alphanumerics, dashes, underscores, slashes).

For example, the tags for this article would be written as:

```subtext-extended
! haskell
! hypertext
! programming
```

These provide the same functionality as hashtags in most web apps, like X / Twitter, but we just call them "tags" since the hashtag sigil character `#` is already used to notate headings.

If you *really* wanted to draw a parallel with "hashtag", I suppose you could call them "bangtags" (given the slang term "bang" for the exclamation point) instead.

### Key-value pairs

**Key-value pairs** let us attach names (the **key**) to data (the **value**) in our documents.

Key-value blocks also start with `!`, much like tag blocks. 

This simplifies writing Subtext Extended, since users don't need to remember another sigil character, even though it makes parsing a little more complicated.

Instead, the way we distinguish between tags and key-value pairs is the presence of some whitespace block which delimits keys from values.

I think this is a good tradeoff, since tags are typically only one-word (that is, no whitespace) anyways in most software (e.g., like hashtags). 

Unfortunately, that means that keys can't contain whitespace, but the alternative was to use a secondary sigil character in the middle of the line. 

In another world where we used a separate sigil character (let's use `%` since there's two dots to represent the 1. key, 2. value) to denote key-value pairs, with a secondary sigil character (let's use `^` as an upside-down "v" for "value") to delimit keys from values, we'd have something like:

```subtext-extended
% key^value
```

I'm not opposed to that solution in general---after all, I do use secondary sigil characters to notate different kinds of transclusion---but I don't think the extra complication in user's minds is worth it in this case.

Syntactically, tag blocks are a sequence of:

- Sigil character `!`
- An optional amount of whitespace
- A nonzero number of non-whitespace characters---e.g. alphanumerics, dashes, underscores, slashes---to represent the **key**
- A nonzero amount of whitespace (the delimiting whitespace)
- A nonzero amount of any characters to represent the **value**

Even though keys can't contain whitespace, values can contain as much as we'd like!

This is how key-value pairs look in Subtext Extended:

```subtext-extended
! key value
! hamlet_monologue_line_1 To be, or not to be, that is the question:
```

### Triples

The final metadata component, **triples**, let us upgrade key-value pairs by encoding a relationship (the **predicate**) between a **subject** and an **object**.

These are basically a port of [RDF's semantic triples](https://en.wikipedia.org/wiki/Semantic_triple).

Triple blocks start with `&`. The three parts of a triple---subject, predicate, and object---are delimited by spaces. 

Similarly to key-value pairs, that means that the subject and predicate can't contain spaces, while the object can.

Syntactically, triples are a sequence of:

- Sigil character `&`
- An optional amount of whitespace
- A nonzero number of non-whitespace characters to represent the **subject**
- A nonzero amount of delimiting whitespace
- A nonzero number of non-whitespace characters to represent the **predicate**
- A nonzero amount of delimiting whitespace
- A nonzero amount of any characters to represent the **object**

Some example triples:

```subtext-extended
& subject predicate object
& haskell is_a programming language
```

## Adding transclusion

Besides metadata, I also added **transclusion** to Subtext Extended.

Transclusion refers to a process in which we include some (or all) of the content of another document inside a document, by reference.

Why add transclusion?

Let me briefly direct you to a relevant excerpt from the [Structure and Interpretation of Computer Programs](https://sarabander.github.io/sicp/html/1_002e1.xhtml):

> A powerful programming language is more than just a means for instructing a computer to perform tasks. 
> The language also serves as a framework within which we organize our ideas about processes. 
> Thus, when we describe a language, we should pay particular attention to the means that the language provides for combining simple ideas to form more complex ideas. 
> Every powerful language has three mechanisms for accomplishing this:
> 
> - **primitive expressions**, which represent the simplest entities the language is concerned with,
> - **means of combination**, by which compound elements are built from simpler ones, and
> - **means of abstraction**, by which compound elements can be named and manipulated as units.

Subtext isn't a programming language by any means.

But it's still a tool for thought and it's got: 

1. **Primitive expressions**: the sub-line-level and line-level block semantics,
2. **A combinator**: concatenation of blocks,
3.  **An abstractor**: attaching a name to a list of Subtext blocks produces a document, which we can reference in other blocks using *slashlinks*.

Adding metadata added new primitive expressions.

Adding transclusion adds a new abstractor.

While slashlinks merely reference a document---meaning the user has to navigate to the document themselves---transclusion lets us document content within a document before it's presented to the user.

That lets us [reuse and remix](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) between our Subtext Extended documents.

### Transclusion syntax

Transclusion blocks start with a `$`, and denote that the content from another document must be fetched and inserted into the location of (and replacing) the block.

Transclusion blocks refer only to local Subtext Extended pages (whose names are akin to slashlinks).

There are 4 options to select what content to refer to in a document:

| Option                                          | Syntax            |
| ----------------------------------------------- | ----------------- |
| All the content of the referenced document      | `$ doc`           |
| The first `n` lines of the referenced document. | `$ doc \| n`      |
| `n` lines after line `m`. Lines are 0-indexed.  | `$ doc \| m n`    |
| The section under a given heading block.        | `$ doc # heading` |

OK, let's make this easier to understand with some examples.

Imagine I have a document named *ode.subtext* which stores some of the poem [Ode by Arthur O'Shaughnessy](https://www.poetryfoundation.org/poems/54933/ode-) (a personal favourite):

```subtext-extended
# Stanza 1

We are the music makers,
And we are the dreamers of dreams,
Wandering by lone sea-breakers,
And sitting by desolate streams; —
World-losers and world-forsakers,
On whom the pale moon gleams:
Yet we are the movers and shakers
Of the world for ever, it seems.

# Stanza 2

With wonderful deathless ditties
We build up the world's great cities,
And out of a fabulous story
We fashion an empire's glory:
One man with a dream, at pleasure,
Shall go forth and conquer a crown;
And three with a new song's measure
Can trample a kingdom down.

# Stanza 3

We, in the ages lying,
In the buried past of the earth,
Built Nineveh with our sighing,
And Babel itself in our mirth;
And o'erthrew them with prophesying
To the old of the new world's worth;
For each age is a dream that is dying,
Or one that is coming to birth.
```

I'll give some example transclusion references below:

#### The starting lines

`$ ode | 3` transcludes the first 3 lines of *ode.subtext*, so the rendered content would be:

```subtext-extended
# Stanza 1

We are the music makers,
```

#### Lines from after the start of the document

`$ ode | 5 4` skips the first 5 lines of *ode.subtext*, then transcludes 4 lines, so the rendered content would be:

```subtext-extended
World-losers and world-forsakers,
On whom the pale moon gleams:
Yet we are the movers and shakers
Of the world for ever, it seems.
```

#### A header section

`$ ode # Stanza 3` references the section under the "Stanza 3" header in *ode.subtext*, so the rendered content would be:

```subtext-extended
# Stanza 3

We, in the ages lying,
In the buried past of the earth,
Built Nineveh with our sighing,
And Babel itself in our mirth;
And o'erthrew them with prophesying
To the old of the new world's worth;
For each age is a dream that is dying,
Or one that is coming to birth.
```

Adding metadata support to Subtextual was pretty easy, so I didn't go into much detail with how I implemented it.

On the other hand, transclusion was a lot more difficult---nearly doubling the size of my codebase---so I'm going to get a bit more into the weeds and discuss how I did it.

### Corpus

### Topologically sorting the references graph