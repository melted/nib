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
The first thing we see is that line comments in Nib uses the familiar `//` line comments, there is no block comment syntax.

A Nib program consists of a series of bindings, with `=` binding names and patterns on the left hand side to an expression on the right hand side. We see here that the same function can be spread over several bindings, which one is used for a particular call is decided by pattern matching against them in order.

Functions are specified using a `<name> <pattern>+` left hand side. Variables can be bound with a left hand side containing a single pattern.

Even when we want to call a function and do not care about the result, we still express it as a binding to `_`, which is a pattern that matches everything and binds no variable.

## Literals

Nib uses the following forms of literals
```
// Integer literal
whole_number = 100

// Float literal
real_number = 19.56

// Char literal
a_letter = 'a'

// bool literal, also true
is_logical = false

// string literal
some_words = "some words" 

// bytearray literal
some_data = #[100, 101, 102]

// named symbol literal
a_symbol = #zzzz
```

## Data representation and types


## Expressions

### Simple expressions

### Binary operators

### Layout

Nib is whitespace sensitive. The offside rule is not as elaborate as in Haskell. The rule is that expressions can never extend to the left of the column that it started in. The other rule is that semicolons in expressions (these appear in cond-expressions, between lambda-clauses and where-bindings) can be substituted with newlines.

```
//      <-- The expression can't go left of this point
fib n = go 0 1 n where
            go a b 0 = b // A semicolon is not needed here since the next binding is on a new line
            go a b n = go b (a + b) (n - 1)

// Notably the optional semicolon can be used to line up the false clauses
// of cond expressions, and chain further ones 
size x = x < 5 => "XS"
         x < 10 => "S"
         x < 15 => "M"
         x < 20 => "L"
         "XL"
```

If an expression starts in column 0, it will never be terminated by the layout rule. It can be terminated by using a pattern or keyword that can't appear in an expression. Like `_` or `module`.
But the best solution is to always start expressions indented. Note that the left hand side of a top level binding is never an expression and not subject to the layout rule and thus can start in column 0 without problems. Bindings in where-clauses can't extend to the left of their parent expression.

```
a =
1 + 1 // inadvisable expression in column 0

// But we can get out of the expression with a pattern that isn't a valid expression
_ = print a
```

## Patterns

## Bindings

