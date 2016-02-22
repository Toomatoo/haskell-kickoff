# Monad Transformers

## Intuition

When dividing zero, the program can blow up because it is an exception.

So for Haskell, we still need Exception Mechanism.

## Implementation

### Maybe

evaluate expression, set return type is `Maybe Int`. Then recursively evaluate
the result.
```haskell
evalMaybe ::  Expr -> Maybe Int
evalMaybe (Val n)   = return n
evalMaybe (Div x y) = do n <- evalMaybe x
                         m <- evalMaybe y
                         if m == 0
                           then Nothing
                           else return (n `div` m)
```
* Another thing here: (revise)

    ```haskell
    instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing
    return              = Just
    ```
    This means, if the parameter is a `Just x`, it will be applied to the
    function afterwards. Otherwise, it will directly return `Nothing`.
    For example,
    ```haskell
    if m == 0
      then Nothing
      else return (n `div` m)
    ```
    is k.
### Exception

First, simply change the return into Exception type.
```haskell
evalExn (Val n)   = return n
evalExn (Div x y) = do n <- evalExn x
                       m <- evalExn y
                       if m == 0
                           then throwExn "EEKES DIVIDED BY ZERO"
                           else return (n `div` m)
```
You can see the data type contains two kinds of constructors, `Exn` and `Result`.

For abstraction, define
```haskell
data Exc a = Exn  String
           | Result a
           deriving (Show)
```
It is the same abstraction with `Maybe`. When applying `Exn` to a function, return
a `Exn` immediately. The instance for `Monad`, `Functor` and `Applicative` is
similar with `Maybe`.

* Revise `Functor` and `Applicative` here

    `Functor` denotes that the data type can be applied to functions. The instance
    need to implement a function `fmap :: (a->b)->f a->f b`.

    For example, for Exc,
    ```haskell
    fmap _ (Exn e) = Exn e
    fmap f (Result r) = Result (f r)
    ```

    `Applicative` need to implement two main functions, `pure` and `<*>`.

    `pure` lifts a variable `a` to `f a`, `<*> :: f (a->b) -> f a -> f b`.

    NOTE: `<*>`'s meaning need to understand.

## Exception

### Throwing Exception
With the implementation above, when `throwExn`, we do not know where exception
happens. So we rewrite it into a complete throwing.

```haskell
evalExc ::  Expr -> Exc Int
evalExc (Val n)   = return n
evalExc (Div x y) = do n <- evalExc x
                       m <- evalExc y
                       if m == 0
                          then throw  $ errorS y m
                          else return $ n `div` m
```
When exception happened, we throw a error with more information.

### Catching Exception

Intuitively, we need to write a tryCatch function, that gets a `Exc` and returns
accordingly. If the `Exc` is a exception, then output the exception information.
Otherwise, return the original results.

```haskell
tryCatch (Exn  err) f = f err
tryCatch r@(Result _) _ = r
```

### State Monad

### Revise State transformer

For a State transformer
```haskell
type StateST = Int
data ST a = S (StateST -> (a, StateST))
```
Here, it means for this `ST` the State for the whole is `StateST`, and the type
of state value is `a`.
```haskell
instance Monad ST where
  return x     = S $ \s -> (x, s)
  (S st) >>= f = S $ \s -> let (x, s') = st s
                               S st'   = f x
                           in st' s'
```
The fast forward for `ST` is to utilize current state `x` into `f` after `st`
and return `st' s'`. I mean like we can write this code:
```haskell
evalE (Var x) = do
                s <- get
                let val = Data.Map.findWithDefault (IntVal 0) x s
                return val
```
`return val` generate a `S st'`, then because it is a fast forward here, the
mechanism will call `st' s'` as a final step which generate a new tuple, BUT
based on VITUAL WHOLE STATE (Remember!). So that it is complete for a fast
forward definition.

Understanding: `ST a` is a function, it accepts a whole state and combine the
current state in order to make it a tuple. Like return, it combine the current
state `x` into a tuple with whole state. Note that I call `s` is virtual state
now, because it is not initialized. As long as it is initialized by runState or
so, the actual state computation will work.

### runST
```haskell
runST :: ST a -> (a, Int)
runST (S st) = st 0
```
In standard API, the funtion is `runState :: State s a -> s -> (a, s)`. It means
you input an initial whole state and get the final state tuple.
