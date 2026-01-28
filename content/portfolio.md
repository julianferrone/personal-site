---
date: "2026-01-28T16:56:12+11:00"
draft: false
title: "Portfolio"
url: "/portfolio"
summary: "A selection of software projects I've worked on."
type: "static"
---

## Forsp VM

I wrote a client-side virtual machine for [Forsp, a Forth/Lisp hybrid lambda calculus language](https://xorvoid.com/forsp.html) in Rust.

The [web app](forsp.julianferrone.com) is purely client-side, written in
WASM + HTML/CSS/JS: the VM is compiled from Rust to WASM and run in a web worker, while
HTML/CSS/JS are used to provide the UI single-page application UI.

The source code can be viewed on [GitHub](https://github.com/julianferrone/forsp).

## Matryoshka

[Matryoshka](https://github.com/julianferrone/matryoshka/) is a composable key-value storage library for Elixir. It allows you to compose together different stores---components which can get, put, and delete values---into a single store.

This allows a few basic components to be combined in many different ways to achieve different effects, like logging, caching, and applying functions to values.

## Subtextual

[Subtextual](http://github.com/julianferrone/subtextual/) is a Haskell parser
for [Subtext](https://github.com/subconsciousnetwork/subtext/tree/main), the
line-oriented, block-based hypertext format designed for notetaking and
composable documents. The parser converts plain-text markup into structured
blocks---e.g. headings, lists, quotes, links---enabling downstream tooling such
as document merging, filtering, backlinking, and transclusion (inclusion of
content into other documents by reference).
