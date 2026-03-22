def fib(n):
    if n == 0: return 0
    if n == 1: return 1
    return fib(n-1) + fib(n-2)

def fast_fib(n):
    def go(a, b, n):
        if n == 0: return a
        if n == 1: return b
        return go(b, a+b, n - 1)
    return go(0,1,n)

print(fib(25))
print(fast_fib(60))
print(fib(20) == fast_fib(20)) 