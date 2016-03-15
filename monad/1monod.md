# Monad


One of the primary developments in the programming language community in recent years (starting in
the early 1990s) has been an approach to integrating the pure and impure camps, based upon the
notion of a “monad”. This note introduces the use of monads for programming with effects in Haskell.


```haskell
class Functor m where
  fmap :: (a -> b) -> m a -> m b
```

There is a typeclass called Functor that corresponds to the type constructors that you can map over.

For example, [] is a type that can be mapped. Actually, [] is an instance of Functor
```haskell
instance Functor [] where
  fmap f []     = []
  fmap f (x:xs) = f x : fmap f xs
```


Sometimes, we want the map can be applied on multiple parameters, like:
lift2 :: (a1 -> a2 -> b) -> Maybe a1 -> Maybe a2 -> Maybe b
lift2 :: (a1 -> a2 -> b) -> IO a1 -> IO a2 -> Maybe b
And we need these parameters are under same class type for map.

For this reason, there is a typeclass called Applicative that corresponds to the type constructors.
```haskell
liftA2 :: Applicative t => (a1 -> a2 -> b) -> t a1 -> t a2 -> t b
```


## Monad

Origin: when you are doing a division computation, there are danger div and safe div. When there
are zero-division happened, the results should be assigned to a special value.
Under this situation, I need to function whose signature is like this:
```haskell
apply :: (a -> Maybe b) -> Maybe a -> Maybe b
```
This means when we are playing with a function (a -> Maybe b), we need to judge the input which
is likely a Maybe a. When it is Nothing, we need to return Nothing, otherwise, it should return
f a.

And the pattern is like a sequence operation. You need to get more variables
from more `Maybe`, then you need a mechanism to wrap them all out. However, You
do not need to write a function with nested tuple type. You just need to make it
a sequence, which get `x1`, `x2`, ..., `xn` out and generate a `Maybe x`.

With abstraction mentioned above, Haskell implements (>>=) function.
```haskell
(>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
m >= f = case m of
    Nothing -> Nothing
    Just x -> f x
```
This means you unwrap a variable out and then put it into a function and maybe
do more wraps before generate a final result.

So that the function of (>>=) is like a function that helps us sequence (处理) the parameter
before putting them into the target function. And the

```haskell
m1 >>= \x1 ->
   m2 >>= \x2 ->
   ...
     mn >>= \xn ->
       f x1 x2 ... xn
```
Then the parameters for f are safe. And I do not need to deal with Nothing parameters.

>It is COOL!

And they have a more readable version:
```haskell
do x1 <- m1
   x2 <- m2
   ...
   xn <- mn
   f x1 x2 ... xn
```
### Make a Type as Instance of Monad
The work for define a instance of Monad is two parts:
1. return
2. >>=

For every `data` type, there are some constructors, like `Maybe` you have `Just`
and `Nothing`, for `List` you have `Empty` and `Bind`.

For **return**, it lift a lower type to a higher Monad type. Here it consists of
characteristics of generics. If the Monad type consists generic type, like `Maybe a`
, you need to define how `a` lifted to `Maybe a`. Beside the work of lifting,
`return` needs to define some logic, like for `Maybe`, `return` lift `a` to `Just a`,
not to `Nothing`. And for `Either`, `return` lift `a` to `Right a` not `Left a`.
(Also, you need to make sure when calling `return a`, `a` is a type needed for `Right`).

Actually, we can see the define of `class Monad` for more details.
```haskell
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```
When defining a Monad, you need a subtype `m` which is a instance of Applicative.
Also, the most important, m must be a type class which accept a type and generate
a type. For example, `Maybe` is a type class actually, it need a type `a` to complete
a define a `Maybe`. And for Monad `Either`, the instance is written like this:
```haskell
instance Monad (Either e) where
    return = Right
    Left  l >>= _ = Left l
    Right r >>= k = k r
```

Why you can only make type class `*->*` to a Monad. The reason is that you only
need to **sequence** one variable once!

We know complete Either is `Either a b`. But we define `instance Monad (Either e)`
here, actually take `b` as a needed type for a complete Either. And logic takes
advantage of it. `b` (Right) is critical content for Either. `return` of Monad
Either takes in a `b`, and return `Right b`.

**Notice** the above description for Monad type define, it is the same with `>>=`.

For **>>=**, we need to define actions for different constructors of the generic
type (`b` for Either) separately. For example, `instance Monad List`, `List` has
two constructors, `Empty` and `Bind`. You need to define different `>>=` for the
two constructors.

### Logic for Defining a Monad

Monad transfer a monad type to a same monad type, from a list to list, from a Maybe
to Maybe, from Either to Either. Emphasis it again: monad type is a type class which
needs to accept one more type to generate a complete data type (no `->`).

The change is the underlying type which is needed to generate a complete data
type, from `List Int` to `List Bool`, from `Maybe Int` to `Maybe Bool`.

### Monad list

[] (list) is an instance of Monad.
```haskell
instance Monad [] where
    return x = [x]
    (x:xs) >>= f = f x ++ (xs >>= f)
```

Apply monad []
```haskell
do x <- [1, 2, 3]
   show x

-- show :: IO ()
-- So that the fast forward is good.
```
This is like a for loop according to list [1, 2, 3] because of Monad [] defination


## State Transformer

```haskell
type ST a = State -> (a -> State)
    return :: a -> ST a
    return x = \s -> (a, s)
```

This means that when return a state value, just bind it with a inputed state.
And this is so COOL! Because the type a is generic, it means type of x could be any type.
```haskell
(>>=) :: ST a -> (a -> ST b) -> ST b
st >= f = \s -> let (x, t') = st t in f x t'
```
This means that a inputed state are supposed to be processed by function f.
Note the generic of State parameter.

### Example: Global Counter

    MORE explain about State: State s a
    1. Instance Declaration
       State  --  is a function of computation     -- (State)     <=>    (+)
       s      --  is a initial value type          -- (State s)   <=>    (1+)
       a      --  is the intermediate value        -- (State s a) <=>    (1+2)
       Therefore, s can be assigned after all the running, and a is the value of for each
       computation step.
    2. The return and bind
       return a = State $\s -> (a, s)
       -- it means a -> b -> (a, b)
       (>>=) :: State s a        ->
                (a -> State s b) ->
                State s b
       -- it is the same as
       (>>=) :: (s -> (a,s))      ->
                (a -> s -> (b,s)) ->
                (s -> (b,s))
       So you can see that our input is State a (which is a partial function that need s)
       then put it into a function that need parameters of previous state value a and state s, in
       order to generate a new State s with a newer state value b.
