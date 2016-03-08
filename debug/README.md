# Haskell Debug Manual

Coding with `Monad` and other mechanism of Haskell, I always want to debug on my code.
I've learnt the basic debug operators with `ghci`.

First Step is to open terminal and type `ghci yourfile.hs`.
```terminal
> ghci yourfile.hs
```

Then, you can set break points, step in/put, continue in `ghci`, which performs much similar with `gdb`.

```terminal
hs> :break <linenumber>

hs> :continue

hs> :show breaks
hs> :delete <breaknumber>

hs> :print <variable name>

hs> :show bindings
```

If you want to print out the variables, you are likely to instantiate the data with `Show`.



