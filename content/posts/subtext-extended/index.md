---
date: '2025-06-27T16:34:42+10:00'
draft: false
title: 'Adding Tags, Triples, and Transclusion to Subtextual'
tags:
- programming
- haskell
- hypertext
publishDate: '2025-08-10'
series: subtext
summary: "I extend Gordon Brander's Subtext with markup for metadata (tags, key-value pairs, and triples) and transclusion."
---

Gordon Brander's Subtext is a pretty simple markup format for hyperdocuments. 

There's no presentation markup to make documents look nice (i.e. there's no CSS analogue). 

There's no programming markup to add reactivity (no JavaScript analogue).

There's only a focused markup for semantics that includes:

- 4 kinds of sub-line-level semantic chunks (the `Inline` data type in Subtextual):
    1. **Plain text**
    2. **Bare URLs** (which Subtextual currently only recognises if they use the HTTP(S) scheme) to external resources
    3. **Angle-bracket-delimited URLs** to external resources
    4. **Slashlinks** to other Subtext documents
- 5 kinds of line-level semantic chunks (the `Block` data type in Subtextual):
    1. **Regular paragraphs**
    2. **Headings**
    3. **Bullet points**
    4. **Quotes**
    5. **Blank lines**

So of course, I decided to ruin that beautiful simplicity by adding a bunch more specifications for line-level semantics---as well as adding parsing for those primitives in my Haskell library [Subtextual](https://github.com/julianferrone/subtextual)---to encode metadata and transclusion.

## Adding metadata

Why add metadata to Subtext?

Subtext documents are designed to be human-readable---and *only* human-readable. 

There's no affordances for machines to read them.

Metadata fixes that by encoding semantic information that symbolic AI (aka [GOFAI](https://en.wikipedia.org/wiki/GOFAI)) can process.

Sure, you could query them with things like LLMs, or search across a collection of them using something like a [term frequency-inverse document frequency search](https://en.wikipedia.org/wiki/Tf–idf) (since they're just text documents at the end of the day), but I'd prefer to add something that a proper reasoner could work with.

You never know. Maybe I'll decide to learn Prolog and I'll write a little reasoner over Subtext documents in it.

I chose to add three kinds of metadata:

1. Tags
2. Key-value pairs
3. Triples

These are unary, binary, and ternary `Block` constructors, respectively.

A tag attaches a **keyword** to a Subtext document, which we can interpret as a category that the document belongs to.

Key-value pairs attach a name (the **key**) to a blob of data (the **value**).

Triples enhance key-value pairs by adding a relationship (the **predicate**) between the key (now called the **subject**) and the value (aka the **object**).

If you squint a little, you can see a sort-of-mapping between the different metadata types, where we keep progressively adding more expressivity:

```goat
                                .-----------.                      
                                | Keyword   | Tag                  
      .-----------.             +-----------+                      
      | Key       |             | Value     | Key-value pairs      
      +-----------+-------------+-----------+                      
      | Subject   | Predicate   | Object    | Triples              
      .-----------+-------------+-----------.                      
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

But I think that sounds horrible, so I won't.

### Key-value pairs

**Key-value pairs** let us attach names (the **key**) to data (the **value**) in our documents.

Key-value blocks also start with `!`, much like tag blocks. 

This simplifies writing Subtext Extended, since users don't need to remember another sigil character, even though it makes parsing a little more complicated.

Instead, the way we distinguish between tags and key-value pairs is the presence of some whitespace block which delimits keys from values.

I think this is a good tradeoff, since tags are typically only one-word (that is, no whitespace) anyways in most software (e.g., like hashtags). 

Unfortunately, that means that keys can't contain whitespace, but the alternative was to use a secondary sigil character in the middle of the line. 

In another world where we used a separate sigil character (let's use `%` since there's two dots to represent the two components of key and value) to denote key-value pairs, with a secondary sigil character (let's use `^` as an upside-down "v" for "value") to delimit keys from values, we'd have something like:

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

So, even though keys can't contain whitespace, values can contain as much whitespace (horizontal only---tabs and spaces) as we'd like!

This is how key-value pairs look in Subtext Extended:

```subtext-extended
! key value
! hamlet_monologue_line_1 To be, or not to be, that is the question:
```

### Triples

The final metadata component, **triples**, let us upgrade key-value pairs by encoding a relationship (the **predicate**) between a **subject** and an **object**.

These are basically a port of [RDF's semantic triples](https://en.wikipedia.org/wiki/Semantic_triple).

Triple blocks start with `&`. 

The three parts of a triple---subject, predicate, and object---are delimited by spaces. 

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

But it's still a tool for thought, and it's got those three mechanisms:

1. **Primitive expressions**: the sub-line-level and line-level block semantics,
2. **A combinator**: concatenation of blocks,
3.  **An abstractor**: attaching a name to a list of Subtext blocks produces a Document, which we can reference in other blocks using *slashlinks*.Adding metadata added new primitive expressions.

Adding transclusion adds a new abstractor.

While slashlinks merely reference a document---meaning the user has to navigate to the document themselves to read it---transclusion lets us include content from another document before it's presented to the user.

That lets us [reuse and remix](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) content in our Subtext Extended documents.

### Transclusion syntax

Transclusion blocks start with a `$`, and denote that the content from another document must be fetched and inserted into the location of (and replacing) the block.

Transclusion blocks refer only to local Subtext Extended pages (whose names are the same as slashlinks---the only characters allowed are alphanumerics, forward slashes, dashes, and underscores).

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

That's the different transclusion option examples done.

Adding metadata support to Subtextual was pretty easy, so I won't go into any detail about how I implemented it.

But transclusion was a lot more difficult---supporting transclusion nearly doubled the size of the Subtextual codebase---so I'm going to get a bit more into the weeds and discuss how I did it.

### Enhancing Blocks with Authored and Resolved wrappers

After adding the metadata types to my Content, I had the following variants for a `Block`, which denotes some line-level piece of content:

```haskell
data Block
  = Paragraph [Inline]
  | Heading Text.Text
  | Bullet [Inline]
  | Quote [Inline]
  | Tag Text.Text
  | KeyValue Text.Text Text.Text
  | Triple Text.Text Text.Text Text.Text
  | Blank
  deriving (Show, Eq)
```

I wanted to be able to statically differentiate transclusion references from raw content that didn't need to be resolved before presenting it to a viewer.

So, rather than further extending `Block` with extra data constructors, I added a new data type for transclusion references, including the different options for how we want to select the content:

```haskell
data Transclusion
  = Transclusion DocumentName TransclusionOptions
  deriving (Show, Eq)

data TransclusionOptions
  = WholeDocument
  | FirstLines Int
  | Lines Int Int
  | HeadingSection Text.Text
  deriving (Show, Eq)
```

And then, to distinguish between what an author has written and what a user can read, I added `Authored` and `Resolved`:

```haskell
data Authored
  = Raw Block
  | ToResolve Transclusion
  deriving (Show, Eq)

data Resolved
  = Present Block
  | ResourceNotFound DocumentName
  | HeadingNotFound DocumentName Text.Text
  deriving (Show, Eq)
```

`Authored` and `Resolved` are a pair of overlapping sets, so I visualise them as a Venn diagram:

```goat
        Authored                                             
         .-----------------------.           Resolved        
        |                .--------+----------------.         
        | Transclusion  |         |                 |        
        | references    |  Block  |   Transclusion  |        
        |               |         |     resolution  |        
         '--------------+--------'          errors  |        
                        |                           |        
                         '-------------------------'         
```

Now, instead of parsing text documents into lists of blocks `[Block]`, I parse them into lists of authored blocks `[Authored]`.

But it's not enough to just parse in `Authored` documents.

I want to be able to resolve transclusion references and insert content before serving it to the user, which is what the `Resolved` type is for.

When we resolve a transclusion reference, we get one of the following outcomes:

1. A list of present blocks `[Present]` which says that the engine was able to resolve the transclusion reference, locate the content, and provide it
2. An error message `ResourceNotFound`, which says the document for the provided document name couldn't be found
3. An error message `HeadingNotFound` which says the document could be found, but the provided header couldn't be found

### Collating a list of Documents into a Corpus

Now that we've got the basic types to support transclusion, it's not enough to work with one Subtext document at a time.

We need to be able to work with collections of documents---since we need to be able to find the transclusion-referenced document---which means we need a new data type.

The [Merriam Webster entry for "corpus"](https://www.merriam-webster.com/dictionary/corpus) provides a name for this type:

> **3a:** all the writings or works of a particular kind or on a particular subject 
>
> *especially*: the complete works of an author

And so I added `Corpus` to store collections of documents:

```haskell
newtype Corpus a = Corpus {
  unCorpus :: Map.Map Core.DocumentName [a]
} deriving (Eq, Ord, Show)
```

From a [denotational semantics standpoint](https://reasonablypolymorphic.com/blog/follow-the-denotation/), you can think of the meaning of `Corpus` as being a lookup function `DocumentName -> [a]`: mapping document names to their content (where `a` refers to some kind of Subtext block).

### Topologically sorting the references graph

Ultimately what we're looking for is some function that takes a collection of written documents, then returns a collection of documents where all the transclusion references have been resolved.

Spiritually, the type signature of that function would look like this: `resolveCorpus :: Corpus Authored -> Corpus Resolved`.

So the ultimate dataflow of the parsing, unparsing, and transclusion resolution looks like this:

```goat
             ┌───────────────────────────┐              
             │ Directory                 │              
             │                           │              
             │ ┌───────────────────────┐ │              
           ┌─┼─┤ Subtext File .subtext ◄─┼─┐            
           │ │ └───────────────────────┘ │ │            
           │ └───────────────────────────┘ │            
           ▼                               │            
        Parsing                        Unparsing        
           │                               ▲            
           │   ┌───────────────────────┐   │            
           │   │ Corpus Authored       │   │            
           │   │                       │   │            
           │   │ ┌───────────────────┐ │   │            
           └───┼─► Document Authored ├─┼───┘            
               │ └───────────────────┘ │                
               └─────────────┬─────────┘                
                             │                           
                             ▼                          
                 Resolving References                      
                       ▲     │                          
                       │     │                          
               ┌───────┴─────▼─────────┐                
               │ Corpus Resolved       │                
               │                       │                
               │ ┌───────────────────┐ │                
               │ │ Document Resolved │ │                
               │ └───────────────────┘ │                
               └───────────────────────┘                
```

Ahh, but I've added a data dependency of resolving references on the `Corpus Resolved`! 

Why?

Well, it's for efficiency.

Imagine we have some nested transclusion references---document A transcludes document B, document B transcludes document C, document C transcludes document D, and so on.

We don't want to re-process document B, C, D, ..., when we process document A---we've already done the work, why repeat ourselves?

Instead, we should:

- Use the `Document Resolved` of D to resolve the `Document Authored` version of C into a `Document Resolved`,
- Then use the `Document Resolved` version of C to resolve B from a `Document Authored` into a `Document Resolved`,
- And continue until we've finished processing the whole chain of documents.

That means we need to work out what order to do the transclusion resolution in.

A quick Wikipedia search will tell us that we're looking for a [topological sort](https://en.wikipedia.org/wiki/Topological_sorting) on the graph of transclusion references:

> In computer science, a **topological sort** or **topological ordering** of a directed graph is a linear ordering of its vertices such that for every directed edge *(u,v)* from vertex *u* to vertex *v*, *u* comes before *v* in the ordering

But thankfully, I didn't need to implement this. The containers package already has [exactly what I need](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Graph.html#t:Graph):

> `topSort :: Graph -> [Vertex]`
> 
> **O(V + E)**. A topological sort of the graph. The order is partially specified by the condition that a vertex i precedes j whenever j is reachable from i but not vice versa.
>
> Note: A topological sort exists only when there are no cycles in the graph. If the graph has cycles, the output of this function will not be a topological sort. In such a case consider using scc.

That last note is important. It means we first need to check if there's any cycles in our graph before we sort it.

But we should've been doing this anyways.

If there's cycles in our graph, that means there's mutual transclusion references.

Mutual transclusion references means that the resolved documents could be infinitely large!

For a quick example, take two Subtext Extended documents, *foo.subtext* and *bar.subtext*:

```haskell
# Foo

$ bar
```

```haskell
# Bar

$ foo
```

Then resolving the transclusion for either document would result in an infinitely large document:

```haskell
# Foo

# Bar

# Foo

# Bar

# Foo
...
```

So when we sort our graph of transclusion references, we need to bubble up an error if there's any cycles, which I do with `GraphContainsCycles` (returning a list of the cycles in the transclusion references graph).

Otherwise, we can return the [directed acyclic graph (DAG)](https://en.wikipedia.org/wiki/Directed_acyclic_graph) of transclusion references as a topologically-sorted list.

```haskell
newtype GraphContainsCycles a = GraphContainsCycles [Graph.Tree a] deriving (Eq, Ord, Show)

sortDag ::
  Graph.Graph -> -- Graph to check cycles for
  (Graph.Vertex -> a) -> -- Lookup vertex names for easier reporting
  Either
    (GraphContainsCycles a) -- Graph has cycles
    [Graph.Vertex] -- topologically sorted list of vertices
sortDag g nameLookup = case cycles g of
  [] -> Right . Graph.topSort $ g
  cycles' -> Left . GraphContainsCycles . fmap (fmap nameLookup) $ cycles'
```

Then, once we've got the list, resolving the corpus is as simple as performing a right fold over the list of names and progressively enhancing the `Corpus Resolved`:

```haskell
addToCorpus :: Corpus Core.Authored -> [Core.DocumentName] -> Corpus Core.Resolved -> Corpus Core.Resolved
addToCorpus auths names resolveds =
  foldr
    (updateResolvedCorpus auths)
    resolveds
    names
  where
    updateResolvedCorpus ::
      Corpus Core.Authored ->
      Core.DocumentName ->
      Corpus Core.Resolved ->
      Corpus Core.Resolved
    updateResolvedCorpus auths name resolveds =
      insertDoc
        (resolveFromCorpuses auths name resolveds)
        resolveds
```

And that wraps up transclusion!

Obviously, I've skipped over a bunch of the other work, like:

- How we build the transclusion references graph by filtering `Document`s down into a list of transclusion references, then extract the document names
- How we excerpt documents using the provided transclusion options

But most of the other code is the classic fitting-together-types work you see in Haskell, and isn't that interesting to discuss---in my opinion, the interesting stuff is what I've discussed:

- My design decisions on adding new data types to the domain: `Transclusion`, `TransclusionOptions`, `Authored`, `Resolved`, `Corpus`
- Using a topological sort to process a `Corpus Authored` into a `Corpus Resolved` efficiently

## Further reading

As always, I've got unit tests written for all this behaviour, and you can see the [latest version of Subtextual at my GitHub](https://github.com/julianferrone/subtextual).