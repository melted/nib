# A Guide to Nib

This is a short guide to the Nib programming language for experienced programmers. Nib is a dynamically typed language in the ML tradition with a syntax heavily drawing from SASL, but quite different semantically.

## Syntax

Here's a simple Nib program
```
// The factorial function
fac 0 = 1
fac n = n * fac (n - 1)

_ = print (fac 10)
```
The first thing we see is that line comments in Nib uses the familiar `//`, there is no block comment syntax.

A Nib program consists of a series of bindings, with `=` binding names and patterns on the left hand side to an expression on the right hand side. We see here that the same function can be spread over several bindings, which one is used for a particular call is decided by pattern matching against them in order.

Functions are specified using a `<name> <pattern>+` left hand side. Variables can be bound with a left hand side containing a single pattern.

Even when we want to call a function and do not care about the result, we still express it as a binding to `_`, which is a pattern that matches everything and binds no variable.

## Literals

Nib uses the following forms of literals
```
whole_number = 100 // Integer literal
real_number = 19.56 // Float literal
a_letter = 'a' // Char literal
is_logical = false // bool literal, also true
some_words = "some words" // string literal
some_data = #[100, 101, 102] // bytearray literal
a_symbol = #zzzz // named symbol literal
```

## Data representation and types


## Expressions

### Simple expressions

### Binary operators

### Layout

## Patterns

## Bindings

