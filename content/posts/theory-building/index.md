---
date: '2025-07-23T12:35:06+10:00'
draft: true
title: 'The Theory-Building Philosophy of Software'
tags:
- programming
publishDate: '2025-08-23'
summary: ""
series: theory-building
---

## Reference Material

> Pick up quotes from https://pages.cs.wisc.edu/~remzi/Naur.pdf

## Outline

- Theory-building
- Peter Naur quote
- Haskell example - easy type creation leads to being easy to represent domains
- Types: encode information about different kinds of things in a domain
- Tests: encode **expected** behaviour (i.e. expected = what humans think should happen in domain)
    - Unit tests: we simulate one set of inputs in our mind
    - Property tests: encode invariants about the domain
- Functions: encode transformation of information