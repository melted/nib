# How should custom pattern matchers be registered?


# Is there a need to lift patterns from expressions?

# Do I need short-circuiting `and` and `or`?

I can do this:
a && b = a => b; false
a || b = (x where x = a) => x; b

Which is clunky.

With lambdas:

(||) a _ | a = a
(||) _ b = b ()

a || { y }

a && b | a = b ()
_ && _ = false

No need for it right now, nice to haves can wait.