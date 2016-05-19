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
This is actually implementaion for Exception deriving from `Maybe`. We replace
`Nothing` with `Exn` which takes the error message.

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

throw = Exn
errorS y m = "Error dividing by " ++ show y ++ " = " ++ show m
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

Count the number of division when evaluating a expression.
```haskell
evalST           :: Expr -> ST Int
evalST (Val n)   = return n
evalST (Div x y) = do n <- evalST x
                      m <- evalST y
                      tickST
                      return (n `div` m)
```
For this `ST` the state is `Int` denoting tick time, the return value is `Int`
denoting the evaluation value. But the problem here is that if `n div m` errors,
then the program crash, and no ticks is recorded.

### runST
```haskell
runST :: ST a -> (a, Int)
runST (S st) = st 0
```
In standard API, the funtion is `runState :: State s a -> s -> (a, s)`. It means
you input an initial whole state and get the final state tuple.


## Monad Multitask
It seems that I can only use one monad. The direct solution is to implement a
complex monad data type, and put the all the features needed into this monad.
But it is obviously a bad idea. If you have add more features into the monad,
it is hard to accomplish it.

The mechanism to do this is to **use monad to define monad**, then the new monad
is a set of features you want to add. When you get several new monads, you can
decorate the monads together as a bunch of features.

### Step 1: describe monad with special feature
```haskell
class Monad m => MonadExc m where
  throw :: String -> m a
class Monad m => MonadST m where
  runStateST :: m a -> StateST -> m (a, StateST)
  getST      :: m StateST
  putST      :: StateST -> m ()
```
> revise `m ()` here
> Because `putST` modifies the whole state with a `StateST`, the return type
> does no work with current state `a`. Therefore, we write the return type is
> `m ()`.

Notice that **use monad to define monad**, you first need the type `m` a `Monad`
which wants to be a instance of `MonadST`.

### Step 2: Using monad with special feature
```haskell
evalMega (Val n)   = return n
evalMega (Div x y) = do n <- evalMega x
                        m <- evalMega y
                        tickST
                        if m == 0
                          then throw $ errorS y m
                          else return $ n `div` m
```
For a simple code using both features - `tickST` modifies ticks, `throw` and
`return` modifies current state.

Here it includes basic implementation of Monad. `tickST`, `throw` and
`return` are all actions to generate a `Monad b` (`>>= :: M a -> (a -> M b) -> M b`).
So that `evalMega` need the `ST` running here contains all the features from
`MonadExc` and `MonadST`.

The type of `evalMega` is `(MonadST m, MonadExc m) => Expr -> m Int`

### Step 3: Injecting Special Features into Monads
This part is actual how the Haskell deal with multiple Monads. The situation is
that when we have two monad, then how to decorate it together and keep both of
the features.

For this part, the lecture actually gets a solution to inject a monad into
another monad, like inject `ExcT` (Exception) into `STT`(Tick).

### Step 4: Preserve Old Features of Monads

For example, You can promote a Monad back to the monad inside it, like:

```haskell
instance MonadExc m => MonadExc (STT m) where
  throw s = promote (throw s)
```

### Step 5: Put together and Run

With the injection mechanism above, first you can define
```haskell
type StEx a = STT  Exc a
type ExSt a = ExcT STT a
```

Then you can use `tick` `throw` and etc at the same time.

## The Monad Transformer Library

### MonadError
`MonadError` is a type class that has function `throwError` and `catchError`.

`ErrorT e m` and `Either e` is a instance of `MonadError`.

### MonadState
`MonadState` is a type class that has function `get` and `put`.

`State s` is a instance of `MonadState`.

### Utilize the Monads
```haskell
tick :: (MonadState Int m) => m ()
tick = do {n <- get; put (n+1)}

eval1 :: (MonadError String m, MonadState Int m) => Expr -> m Int
eval1 (Val n)   = return n
eval1 (Div x y) = do n   <- eval1 x
                     m   <- eval1 y
                     if m == 0
                        then throwError $ errorS y m
                     else do tick
                                return  $ n `div` m
```

Here we get the evaluation function, from expression to a Monad value. But how
we can actually run the evaluator?

First we define a stacked monad.
```haskell
-- It is mainly a error type, the return value is `a`
type ES a = ErrorT String (State Int) a
```
Second, we define a `run` function
```haskell
-- running must correspond to the wrap order of `ES`
runES :: ES a -> (Either String a, Int)
runES m = runState (runErrorT m) 0

ghci> runES (evalES ok)
(Right 42,2)

ghci> runES (evalES err)
(Left "Error dividing by Div (Val 2) (Val 3) = 0",2)
```
So that it first unwrap the outer layer `ErrorT` of the monad `ES`. Then unwrap
`State`. Then the final `(a, s)` type is `(Either String a, Int)`. We can get
evaluation result and tick value (it is the return value of `State`)at the same
time.

However, you can stack them in another order.
```haskell
type SE a = State Int (Either String) a

runSE :: SE a -> Either String (a, Int)
runSE m = runStateT m 0

ghci> runSE (evalSE ok)
Right (42,2)

ghci> runSE (evalSE err)
Left "Error dividing by Div (Val 2) (Val 3) = 0"
```
**the stack order is very important**, and the result is related with it.
For `SE`, we first unwrap `State` and get `(a, Int)`. Then unwrap `Either` and
get `Left` or `Right`. Under this situation, if the result is a `Left`, the
result from unwrapping `State` will be discarded.

Therefore, the `ErrorT` or `Either` is usually on the top of the stack, which
makes sure that the sequence unwrapping will not discard other information.

Note: `run*T` if there is a T, it means it run a Monad Transformer, and return a
monad. Like `runSE` uses `runStateT`. It means it return a `Either String (a, Int)`.

## Some Function for Running State

**execState** :: State s a -> s -> s

**evalState** :: State s a -> s -> a

**runState** :: State s a -> s -> (a, s)
