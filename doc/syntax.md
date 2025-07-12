Let's make a BNF grammar

```
Program := decl*
decl := `module` name | `use` name | binding
binding := binding_lhs `=` expr
binding_lhs := pattern | name pattern+ [`|` expr] | pattern binop pattern [`|` expr] | `do`
pattern := literal | name | `_` | `[` [pattern `,`]* pattern `]` |
          pattern `@` identifier | `(` name pattern+ `)` | `...` name
  
expr := literal | identifier | expr (expr)+ | expr binop expr | `(` expr `)` | `[` (expr `,`)* expr `]` |
        `{` ((clause ;nl)* clause | expr ) `}` | expr `where` (binding ;nl)* binding | expr `=>` expr ;nl expr |
        expr `.` expr 

;nl := `;` | <newline>

clause := pattern+ [`|` expr] `->` expr

literal = nil | `true` | `false` | string | char |
          number | symbol | `#[` (u8 `,`) u8 `]`


nil = `nil` | `()`

identifier = (alpha | `_`) (alphanum | `_`)* and not in reserved_words
| `(` binop `)`

binop = opchar+ not in reserved | `\`` name `\`` 

name = (identifier `.`)* identifier

opchar = unicode symbol except '{} [] ().,;\'\"'
reserved_symbols = `=` `@` `->` `_` `=>` '|'
reserved_words = `module` | `use` | `where` | `true` | `false` | `_`
```
---

The core language desugars binops to applications, fun and op bindings to var binding of lambdas. Arrays desugar to application of the array constructor, projection desugars to an application of a projection primitive