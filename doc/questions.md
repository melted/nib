# How should custom pattern matchers be registered?

Idea: They don't need to be, just make a function called that, that takes a value (and it also needs to know how many outputs are expected, for those that can handle several possibilities), it should do computations and output an array with the number of fields that the pattern contains, or false if it's not a match. The pattern matcher will then continue to match the fields to the outputs.


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

# Is Nib really a good name for the language?

I don't know, the repo is named nibble for random reason, because it had to have a name. And nibble is as good anything. Maybe there is some allusion there to something less than bite-sized. But there certainly is no deep thought put into it.

Let's see, I quite like Nib. It's short and sweet.
