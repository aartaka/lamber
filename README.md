# Lamber, a Simple Language

Lamber is a minimalist functional programming language with a focus on graspability, readability...
and compilation to pure untyped Lambda Calculus, of course!
It's inspired by Lua, Haskell, and Wisp.
And it smells of creamy cheese!

## Lamber in a Postcard

``` lua
def factorial function (n)
  if (= n 0)
   then 1
  else
   * n : factorial : dec n .
```

Step by step:
- You can bind things to names/variables (like `factorial`) with `let`, `def`, `local`, and `var`. All of these do the same thing, so pick the one you like the most.
- Functions are all there is to Lamber. It compiles to raw lambdas, after all. It's only consequential that things you bind to vars are `fn`s, `lambda`s,a nd `function`s.
- There's `if`/`when` to do conditional checks. With `else` branches and `else if` chaining. Boring stuff, right?
- There's no need for explicit `return` statement, because the last form in the function/branch is the return value.
  - So factorial returns either `1` or... wait, I need to explain some more.
- Functions are prefix in Lamber, so `= n 0` means "n is equal to zero" and `dec n` means decreasing n.
- There's no looping in Lamber, only recursion.
  - Luckily, first-class functions and abundance of list processing functions in the standard library make explicit looping unnecessary in most cases.
- There are several special syntax pieces, mostly stolen from [Wisp](https://srfi.schemers.org/srfi-119/):
  - `:` colon to chain function calls. Lamber emphasizes composable functions that are easy to chain, as in `* n : factorial : dec n`:
    - Decrease n.
    - Call `factorial` on it.
    - And multiply the result by `n`.
  - `.` period is a shortcut for `end` in Lua and the kind. You can do explicit `end` too, but it's not as pretty:
``` lua
  else
   * n : factorial : dec n
end
```

## Installation & Getting Started

- Clone the repo:
``` sh
git clone --recursive https://github.com/aartaka/lamber
```
- Install a Lisp implementation like [SBCL](https://sbcl.org/).
- And compile the `lamber` executable with
``` sh
make
```
- Use the generated `lamber` as in
``` sh
./lamber example/hello.lmb string
# Hello, World!
```
