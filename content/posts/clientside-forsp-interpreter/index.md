---
date: '2026-01-25T18:52:58+11:00'
draft: false
title: 'Interpreting Forsp in Rust'
tags:
- programming
- rust
publishDate: '2026-01-26'
summary: "I write a client-side REPL for Forsp, a hybrid Forth/Lisp
lambda calculus language, in Rust."
---

I've wanted to learn how to write an interpreter for quite a while now, just as
a neat learning project. Sure, I'd get practice embedding a higher-level
language in something lower-level, but the real goal is to be able to experiment 
with building [malleable systems](https://malleable.systems/), or play around
with ideas for toy languages.

Both Forth and Lisp have been in my crosshairs for this purpose---their austere
syntaxes mean I can write a fully fledged interpreter without wasting weeks of
time and energy implementing the parser, and I can just focus on the core
semantic functionality.

But I was always stuck. So many choices! Do I write a Forth? Do I write a Lisp?
I'd been inspired to write an interpreter by seeing [Dave Gauer's NASM port of
JONESFORTH](https://www.ratfactor.com/nasmjf/), so do I bother writing it in
Assembly?

But then, a couple weeks ago, I came across [xorvoid's Forsp: A Forth+Lisp
Hybrid Lambda Calculus Language](https://xorvoid.com/forsp.html), and my ideas
started falling into place.

You can try out [the Forsp REPL in your web
browser](https://forsp.julianferrone.com). It's 100% client-side!{{< sidenote
>}}The server that I run this website on is barely powerful enough to run a
static site.{{< /sidenote >}}

## Decisions, Decisions, Decisions

The very first seeds that germinated into this project, borne of a fit of
madness, were a brief compulsion to write a Forth in WASM.

By hand.

WASM was a natural choice because I wanted to write something that ran locally,
across multiple OSes, but the "by hand" part was pure ego talking. I'm not
enough of a masochist for the nerd cred{{< sidenote >}} And my education by fire
in the workings of the WASM VM.{{< /sidenote >}} of writing everything by hand
to be worth it.

Now, I still chose to deploy the interpreter in WASM. But instead of doing
everything in pure WASM like some sort of maniac, I opted to write the
interpreter in Rust and compile to WASM instead.

And I didn't want to just write an interpreter, I wanted to write a REPL. I
figured it wouldn't add that much more development time---just gluing some kind
of UI over the core read/eval/print functions---and it'd make playing with the
system a lot more fun.

This was the other reason I picked Rust compiled into WASM---the universal GUI,
these days, is the web browser. My plan was to write the interpreter in Rust,
wrap the compiled WASM in a JS [web
worker](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers)
to do the interpretation work in a separate thread (so we didn't block the UI),
spin up a quick little interface in HTML, style it with some CSS to look like a
terminal, and hey presto we'd have a cross-OS Forsp REPL!

That would also solve my portability goal---I wanted to write a REPL that fit
inside one file, so users could could just save the HTML file to their desktop,
open it in their browser, and then the REPL is ready to go. 

## The code

I don't want to go over every last line of code---the source code is [available
to see on my GitHub](https://github.com/julianferrone/forsp) if you're
interested in that---but I do want to spend a little time discussing the types
of the code and some of the decisions I made.

### Atoms

Atoms are pretty self-explanatory---they work the same as Lisp atoms.

They're either numbers, which I've represented as 64-bit integers, or symbols,
which are used as identifiers.

```rust
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Atom {
    Symbol(String),
    Num(i64),
}
```

Annoyingly, I can't use the `#[wasm_bindgen]` macro to let me serialize `Atom`s
across the WASM ABI. I'm having to use
[serde](https://wasm-bindgen.github.io/wasm-bindgen/reference/arbitrary-data-with-serde.html)
to serialize and deserialize them into JSON and send them to the web worker,
which is slow and inefficient, but at least it works.{{< sidenote >}} If I were
willing to give up some type safety, I could hand-compile `Atom`s as structs
with a type field, like so:

`pub enum AtomType {Symbol, Num}`

`pub enum Atom {typ: AtomType, symbol: Option<String>, num: Option<i64>}`

Which would let me send them across the WASM ABI, but then I'd have to keep
track of the invariants myself, like ensuring that only one of `atom.symbol` and
`atom.num` contain a value at any given time.

It'd also take a lot more memory since the Atom type would be as large as the
sum of the sizes of `String` and `i64` rather than as large as the maximum.
{{< /sidenote >}}

There's currently [an open issue about
the fact that wasm bindgen doesn't support enum variants with associated
data](https://github.com/wasm-bindgen/wasm-bindgen/issues/2407), so hopefully
that'll be fixed one day and I can swap out serde for `#[wasm_bindgen]`.

### S-Expressions

The other main Lispism is pairs, out of which we build everything. 

Lisp deals with symbolic expressions, aka S-expressions, which
are either an atom, or a pair of s-expressions `(a . b)`.

Lists are pairs.

To create a list, we nest pairs such that the second part of the pair is either
a list, or the special atom `NIL` (which denotes the end of the list), like `(1
. (2 . (3 . NIL)))`. But to make things nicer to read, Lisp prints these
"proper lists" in just one set of parentheses:

`(1 2 3)`

Trees are pairs.

To create a tree, we allow the left-hand sides of pairs to be another list:

`((1 2) (3 4))`{{< sidenote >}}`((1 . (2 . NIL)) . (3 . (4 . NIL)) . NIL))` {{<
/sidenote >}}

Key-value maps are pairs. 

An [association list](https://en.wikipedia.org/wiki/Association_list), or
assoc-list, is a list of key-value pairs. When we want to look up a value in the
assoc-list, we traverse the list until we find a pair whose left-hand side is
our key, then return the right-hand side of the pair, which is the value.

To create an association list, we ensure that the leftmost heads of
the pairs are themselves pairs---not proper lists that
end in NIL, like trees, but just pairs of keys and values:

`((foo . 1) (bar . 2) (baz . 3))`{{< sidenote >}}`((foo . 1) . ((bar . 2) .
((baz . 3) . NIL)))`.{{< /sidenote >}}

The first time I wrote a type for S-expressions, I wanted to be faithful to this
original representation, which originated from
[1950s era memory constraints](https://en.wikipedia.org/wiki/CAR_and_CDR). 

```rust
#[derive(Debug, PartialEq, Clone, Deserialize, Serialize)]
pub enum Sexpr<T> {
    Nil,
    Single(T),
    Pair(Box<T>, Box<T>)
}
```

I will admit this was a monumentally stupid idea.

First off, the memory use seemed wasteful---every extra entry in a `Sexpr` list
would take another `size(T) + tag discriminant` bytes to store---and traversing
through a list would require indirection through Box pointers for every item in
the list.

Implementing traits like
`Display` was even worse since I had to hand-roll stack-based pretty printers to
convert the pairs into nice parenthesized `(a b c)` lists.

Thankfully I came to my senses and later reworked Sexpr to use a more Rusty
representation (as part of my efforts to write a virtual machine for Forsp{{<
sidenote >}} The current Forsp REPL already uses the VM instead of the
tree-walking interpreter. Fodder for a future post. {{< /sidenote >}}):

```rust
#[derive(Debug, PartialEq, Clone, Deserialize, Serialize)]
pub enum Sexpr<T> {
    Single(T),
    List(Vec<Box<Sexpr<T>>>),
}
```

Granted, we lose the ability to represent naked pairs, but this felt like a much
better representation---not having to track `NIL`s, or checking if we're working
with improper or proper lists. 

So now Forsp only has two kinds of S-expressions: single items, and lists of
S-expressions.

### Values

Now that we can work with generic symbolic expressions, we need to specialise
them with a type for our interpreter to run on:

```rust
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Value {
    Atom(Atom),
    Closure(Box<Value>, Box<Value>),
    Primitive(Primitive),
    Sexpr(Box<Sexpr<Value>>),
}
```

A `Value::Atom` just contains an `Atom::Symbol` or `Atom::Num`, that's
self-explanatory.

And a `Value::Sexpr` contains a list of Values{{< sidenote >}} We could
technically have it point at a `Sexpr::Single(Value)`, which would be the same
semantically as a `Value::Atom`, but the implementation of `From<Sexpr<Value>>
for Value` takes the underlying `Value` out from a `Sexpr::Single`. Only
`Sexpr::List` gets boxed into a `Value::Sexpr`.

I'm pretty sure you'd need dependent typing to enforce this statically, but a
smart constructor is more than good enough for my purposes.
{{< /sidenote >}}.

But that leaves `Closure`s and `Primitive`s. Both of these are basically
functions.

`Closure`s are composed of a function body and an environment. When we evaluate
a closure, we use the closure's environment instead of the interpreter's
environment, and treat the function body as the program to run.

`Primitive`s are a set of primitive functions built into the REPL that the user
can use to create their own functions.

In fact, `Primitive`s were originally just a wrapper around a function from an
interpreter state and a value to either a state or an error:

```rust {hl_lines=[5]}
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Value {
    Atom(Atom),
    Closure(Box<Value>, Box<Value>),
    Primitive(fn(State, Value) -> Result<State, String>),
    Sexpr(Box<Sexpr<Value>>),
}
```

This runs into an issue, though. 

We need to be able to serialize and
deserialize Values, but you can't do that on arbitrary functions{{< sidenote >}}You
can treat ASTs as data that we can read, write, save, load, but that's basically
what I did.{{< /sidenote >}}, so I [defunctionalised
them](https.//www.pathsensitive.com/2019/07/the-best-refactoring-youve-never-heard.html)
and turned primitives into a data structure.

Now, whenever we need to evaluate a primitive, we pattern-match to find the
function to apply to the interpreter state{{< sidenote >}}The other alternative
was to not have primitives as a datatype at all. 

I could've set up a huge match
statement in `eval` that would check a `Symbol` against a list of predefined
primitive names and then dispatched to apply the relevant functions, but that
seemed a lot less clean. 

It also would've meant the primitive functions wouldn't
have been in the base interpreter environment, which I didn't like.{{<
/sidenote >}}:

```rust
pub enum Primitive {
    Push,
    Pop,
    Equals,
    ...
}

fn apply_primitive(self: State, primitive: Primitive) -> State {
    let func = match primitive {
        Primitive::Push => prim_push,
        Primitive::Pop => prim_pop,
        Primitive::Equals => prim_equals,
        ...
    }
    ...
}
```

Now that we have the values defined, we need to be able to interpret them. 

### Interpreting values

```rust
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct State {
    pub stack: Value,
    pub env: Value,
    pub messages: Sexpr<String>,
    pub err_messages: Sexpr<String>,
}
```

The interpreter consists of four things:

1. A stack of values to operate on
2. Our environment (represented as an association-list) which maps
`Atom::Symbol`s to `Value`s
3. A stack of standard messages to print out
4. A stack of error messages to print out

The message stacks let us keep the `print` primitive pure---we want to be able
to either print the messages to STDOUT/STDERR if we're running the REPL in the
terminal, or have the web worker access the messages if we're running the REPL
via the web app.

The core functionality of the interpreter is in the methods `compute` and
`eval`.

`compute` runs a program, which is an s-expression of `Value`s:

```rust
pub fn compute(self, program: Sexpr<Value>) -> State {
    let mut state = self;
    let mut comp = program;

    loop {
        match comp {
            Sexpr::Nil => return state,
            Sexpr::Single(obj) => return state.eval(obj),
            Sexpr::Pair(ref cmd_box, ref rest_box) => {
                let rest = *rest_box.clone();
                let cmd_value: Value = (*cmd_box.clone()).into();
                
                // Check if we're seeing the "quote" special form
                if let Value::Atom(
                    Atom::Symbol(ref name)
                ) = cmd_value {
                    if name == "quote" {
                        match rest {
                            Sexpr::Pair(quoted, tail) => {
                                let quoted: Value = (*quoted).into();
                                state = state.push(quoted);
                                comp = *tail;
                                continue;
                            }
                            _ => {
                                state = state.eprint(
                                    "quote expects an argument"
                                );
                                continue;
                            }
                        }
                    }
                }
                state = state.eval(cmd_value);
                comp = rest;
            }
        }
    }
}
```

The only interesting thing in `compute` is that it's where we process the
special `quote` form by pushing the next value to the top of the
stack. 

Otherwise, we evaluate the current value via `eval`:

1. `Symbol`s get looked up in the environment
    1. `Primitive`s get evaluated
    2. `Closure`s get computed
    3. `Atom`s and `Sexpr`s get pushed to the top of the stack
    4. If there's no value found, print an error message
2. `Num`s get pushed to the top of the stack
3. `Sexpr::List`s are transformed into closures by bundling them with the current
interpreter's environment, then pushed to the top of the stack
4. Any other values get pushed to the top of the stack

```rust
pub fn eval(self, expr: Value) -> State {
    match expr {
        Value::Atom(ref atom) => match atom {
            // 1. Symbols get looked up in the environment
            Atom::Symbol(_) => {
                let value = env_find(&self.env, &expr);
                match value {
                    // 1.1. Primitives get applied
                    Ok(Value::Primitive(func)) => {
                        self.apply_primitive(func)
                    },
                    // 1.2. Closures get computed
                    Ok(Value::Closure(body, closure_env)) => {
                        let saved_env = self.env.clone();
                        let state = self
                            // Change the state's environment to the
                            // closure's environment
                            .with_env(*closure_env)
                            // Compute the state using the closure's 
                            // program body
                            .compute((*body)
                            .into());
                        // Restore the original interpreter's 
                        // environment
                        state.with_env(saved_env)
                    }
                    // 1.3. Push other values to the top of the stack
                    Ok(object) => self.push(object),
                    // 1.4. Print an error message if we can't find the 
                    // value
                    Err(err) => self.eprint(err),
                }
            }
            // 2. Numbers get pushed to the top of the stack
            Atom::Num(_) => self.push(expr),
        },
        // 3. Make lists into closures and push to the stack
        Value::Sexpr(ref _sexpr) => {
            let closure = Value::make_closure(expr, self.env.clone());
            self.push(closure)
        }
        // 4. Push other values to the top of the stack
        other => self.push(other),
    }
}
```

Now that we have evaluation working, it's time to get the reading part of the
read-eval-print-loop working.

### Parsing user input

To make parsing easier, I split up the parsing into a pipeline composed of:

1. A scanner, which produces a deque{{< sidenote >}}The scanner pushes Tokens to
the back of the queue, while the reader needs to pop Tokens off the front of the
queue.{{< /sidenote >}} of tokens 
2. A reader, which produces a program for the
interpreter to compute{{< sidenote >}}: The reason `read` returns a
`Sexpr<Value>` rather than just a `Value` is because I want the user to be able
to type in multiple instructions at once and have the interpreter interpret each
in turn. That means I always need to parse the user input in as a list of 
instructions, even if they only type one atom in.{{< /sidenote >}}

```goat
.------.              .-------.           .------.           .-------------.
| User +------------>| Scanner +-------->| Reader +--------->| Interpreter |
'------' User Input   '-------'  Tokens   '------'  Program  '-------------'
           
           String            VecDeque<Token>      Sexpr<Value>
```

`scan` tokenises the string one character at a time. The only intelligent
thing it does is convert numeric tokens into integers straight off the bat,
while leaving all other alphanumeric tokens as literals.

`Token`s look like this:

```rust
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Quote,
    Caret,
    Dollar,
    Semicolon, // Skip line-comments
    BracketOpen,
    BracketClose,
    WhiteSpace,
    NewLine,
    Int(i64),
    Literal(String),
}
```

`read` does the heavy lifting of converting bracketed expressions into lists,
ignoring whitespace and comments, and processing the special forms `' $ ^` by
splicing the relevant primitives in:

- `'x` becomes `quote x`
- `$x` becomes `quote x pop`
- `^x` becomes `quote x push`


This [separation of concerns makes writing the parser a lot
easier](https://parentheticallyspeaking.org/articles/bicameral-not-homoiconic/).

Well, that's reading and evaluation done. Now it's time to add the print part of
the REPL by building the web UI.

### The web UI

The web UI is a thin wrapper around the interpreter.

First things first, we'll need to provide the web worker a way to create a new
interpreter as a JavaScript value, which we do via serde:

```rust
use wasm_bindgen::prelude::*;

mod interpreter;
mod parser;
mod sexpr;

use crate::interpreter::{State, Value};
use crate::parser::{read, scan};
use crate::sexpr::Sexpr;

#[wasm_bindgen]
pub fn new_state() -> JsValue {
    let state = State::new();
    serde_wasm_bindgen::to_value(&state)
        .expect("Should be able to serialize new state via serde")
}
```

As well as a `repl` function to parse the user input and return a tuple of:

1. The interpreter state
2. A list of messages for the web worker to print
3. A list of error messages for the web worker to print

```rust
pub fn repl(state: State, user_input: &str) -> (State, Sexpr<String>, Sexpr<String>) {
    let parsed = read(scan(user_input));
    match parsed {
        Ok(atoms) => {
            let exprs: Sexpr<Value> = atoms.into();
            return state.compute(exprs).flush_messages();
        }
        Err(err) => {
            return (
                state,
                Sexpr::Nil,
                Sexpr::cons(Sexpr::Single(err), Sexpr::Nil),
            )
        }
    }
}
```

The `repl_js` function just wraps the `repl` by serializing and deserializing
between JavaScript values and the Rust values:

```rust
#[wasm_bindgen]
pub fn repl_js(state: JsValue, user_input: JsValue) -> Result<JsValue, JsValue> {
    let state: State = serde_wasm_bindgen::from_value(state)?;
    let user_input: String = serde_wasm_bindgen::from_value(user_input)?;
    let (new_state, msgs, error_msgs) = repl(state, &user_input);
    let msgs: Vec<String> = msgs.into();
    let error_msgs: Vec<String> = error_msgs.into();
    let result = (new_state, msgs, error_msgs);
    let result_js = serde_wasm_bindgen::to_value(&result)?;
    Ok(result_js)
}
```

The web worker is a tiny wrapper around `repl_js`. Whenever it receives a
message, it computes the new state by interpreting the input, then posts a
message back to the main thread with two lists of messages to print: standard
and error.

```js
importScripts('./pkg/forsp.js');

console.log("Initializing worker")

const { new_state, repl_js } = wasm_bindgen

async function init_wasm_in_worker() {
    await wasm_bindgen('./pkg/forsp_bg.wasm')

    var state = new_state();

    self.onmessage = async (event) => {
        console.log(`Received event: ${event.data}`);
        var result = repl_js(state, event.data);
        console.log(`Calculated new state: ${result}`);
        state = result[0];
        msgs = result[1];
        errors = result[2];
        console.log(JSON.stringify(state, null, 2));
        console.log(JSON.stringify(msgs, null, 2));
        console.log(JSON.stringify(errors, null, 2));
        postMessage([msgs, errors]);
    }
}

init_wasm_in_worker()
```

Since I wanted the web UI to look like a terminal, the main thread takes those
messages and prints them to the screen by appending new divs (at most, 80
characters wide) to the output element.

```js
function appendLine(text) {
    const line = document.createElement("p");
    line.textContent = text;
    output.appendChild(line);
    output.scrollTop = output.scrollHeight;
}

function appendText(text) {
    if (text.length <= 80) {
        appendLine(text)
    } else {
        const length = 80; // 80 characters in the terminal
        var split = [];
        for (var i = 0; i < text.length; i += length) {
            split.push(text.substr(i, length));
        }
        split.reverse().forEach(line => appendLine(line))
    }
}

worker.onmessage = (e) => {
    const worker_result = e.data;
    console.log(`Message received from worker: ${worker_result}`);
    const msgs = worker_result[0];
    const err_msgs = worker_result[1];

    msgs.forEach(line => appendText(line));
    err_msgs.forEach(line => appendText(`ERR: ${line}`));
}
```

Besides that, the only interesting part in the main web UI is the render
function, which makes the `input` HTML element look like a command-line
terminal: 

```js
function render() {
    const value = input.value;
    const pos = input.selectionStart ?? 0;
    const focused = document.activeElement === input;

    const before = value.slice(0, pos);
    const at = value[pos];
    const after = value.slice(pos + 1);

    rendered.innerHTML = "";
    rendered.append(document.createTextNode(before));

    const cursor = document.createElement("span");

    if (focused && at) {
        cursor.className = "cursor-block";
        cursor.textContent = at;
    } else if (focused && !at) {
        cursor.className = "cursor-eol";
        cursor.textContent = " ";
    } else if (!focused && at) {
        cursor.className = "cursor-unfocused";
        cursor.textContent = at;
    } else if (!focused && !at) {
        cursor.className = "cursor-unfocused";
        cursor.textContent = " ";
    }

    rendered.append(cursor);
    rendered.append(document.createTextNode(at ? after : ""));

    // --- horizontal scrolling ---
    const scrollCols = Math.max(0, pos - INPUT_COLUMNS + 1);
    rendered.style.transform = `translateX(${-scrollCols}ch)`;
}
```

I'm of course glossing over the various other event listeners we need to stitch
together to send the user input to the worker and change how we render the
terminal input (i.e. focus and unfocusing on it), but that's just boiler-plate. 

The main thread waits until the Forsp WASM gets loaded, starts up the web worker
to run the interpreter, starts listening to user input events, and performs the
initial render of the terminal input.

## Future work

My Forsp REPL isn't polished at all. There's a laundry list of improvements I'd
like to make:

I said before that I *wanted* the REPL to be just one file, but that's not done
yet. I'll need to write some kind of build script that packages up the HTML,
CSS, JavaScript, and the WASM blob into one HTML file.

There's not really a way to read in and execute whole scripts at once, so that'd
also be a good idea.

I also want to add the ability to save and load images, ala Lisp, so that you
can save and load the state of the entire Forsp system (i.e., the data stack and
environment) at once.  Right now, if you refresh the browser, you'll reset the
state of the Forsp interpreter and it'll start from scratch. 

It looks terrible on mobile, so it needs some responsive CSS.

Even despite all my todos, though, I'm happy enough to release it.

Once again, you can try out [the Forsp REPL in your web
browser](https://forsp.julianferrone.com), 100% client-side. And as always, you
can see the [latest source code for my Forsp interpreter at my
GitHub](https://github.com/julianferrone/forsp).
