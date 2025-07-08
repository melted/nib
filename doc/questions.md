# How should custom pattern matchers be registered?

Idea: They don't need to be, just make a function called that, that takes a value (and it also needs to know how many outputs are expected, for those that can handle several possibilities), it should do computations and output an array with the number of fields that the pattern contains, or false if it's not a match. The pattern matcher will then continue to match the fields to the outputs.

Problem: Let's say I make a type pair, that contains two fields. Then I want a constructor function named pair : x -> y -> pair, and also a pattern matching function also named pair : v -> [x, y] | false. One solution could be that pair names the type table and the two functions are pair.match and pair.mk respectively. The pattern matcher could implicitly call match if the custom matcher isn't a function. 

# Is there a need to lift patterns from expressions?

I guess by this I meant, is there a need for pattern literals? I haven't run into an acute need yet, so let's hold off until there is a compelling reason to add them.

# Do I need short-circuiting `and` and `or`?

I can do this:
a && b = a => b; false
a || b = x => x; b
            where
                x = a // Not evaluating a twice

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

# Modules are tables, right, how would that work?

# Type tables, what goes into them?

# How is a custom type designated?

# Shouldn't I be able to put an anonymous function in a custom pattern?

Doesn't seem like a mainstream thing, but I guess it could be useful to partially apply some function for just that use site.

Like ({ colorspace #rgb a } r g b)

