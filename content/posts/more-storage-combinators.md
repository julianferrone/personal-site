---
date: '2025-05-05T19:43:01+10:00'
draft: true
title: 'Matryoshka 2: More Storage Combinators'
tags:
- elixir
- programming
publishDate: 2025-05-20
---

Let's add some more stores and store combinators to Matryoshka, my composable storage library in Elixir.

## MappingStore

First off

> The Mapping Store is an abstract superclass modelled after a map() function [37] or a Unix filter, applying simple transformations to its inputs to yield its outputs when communicating with its source. Due to the fact that stores have a slightly richer protocol than functions or filters, the mapping store has to perform three separate mappings:
> 1. Map the reference before passing it to the source.
> 2. Map the data that is read from the source after it is read.
> 3. Map the data that is written to the source, before it is written.



## SwitchingStore
 
## DuplicationStore

## CachingStore



OK so, we can see that we have run into a bit of an issue when it comes to the usability of Matryoshka. All our stores so far have been in-memory only, so we lose all the data when the store closes (i.e. because the store BEAM process terminates). But now that we've implemented CachingStore, we have the ability to cache data using a fast in-memory store and a source on-disk store. So I think it's high time we add stores that persist data to disk.

We'll be doing that in the next post in this series.

You can see the latest version of Matryoshka at [my GitHub](https://github.com/julianferrone/matryoshka).