{-
One of the primary developments in the programming language community in recent years (starting in
the early 1990s) has been an approach to integrating the pure and impure camps, based upon the
notion of a “monad”. This note introduces the use of monads for programming with effects in Haskell.
-}

{-
class Functor m where
  fmap :: (a -> b) -> m a -> m b

There is a typeclass called Functor that corresponds to the type constructors that you can map over.

For example, [] is a type that can be mapped. Actually, [] is an instance of Functor

instance Functor [] where
  fmap f []     = []
  fmap f (x:xs) = f x : fmap f xs

-}

{-
Sometimes, we want the map can be applied on multiple parameters, like:
lift2 :: (a1 -> a2 -> b) -> Maybe a1 -> Maybe a2 -> Maybe b
lift2 :: (a1 -> a2 -> b) -> IO a1 -> IO a2 -> Maybe b
And we need these parameters are under same class type for map.

For this reason, there is a typeclass called Applicative that corresponds to the type constructors.
liftA2 :: Applicative t => (a1 -> a2 -> b) -> t a1 -> t a2 -> t b
-}


{-
Monad
    Origin: when you are doing a division computation, there are danger div and safe div. When there
    are zero-division happened, the results should be assigned to a special value.
    Under this situation, I need to function whose signature is like this:
    apply :: (a -> Maybe b) -> Maybe a -> Maybe b
    This means when we are playing with a function (a -> Maybe b), we need to judge the input which
    is likely a Maybe a. When it is Nothing, we need to return Nothing, otherwise, it should return
    f a.
-}

-- With abstraction mentioned above, Haskell implements (>=) function.
--      (>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--      m >= f = case m of
--                  Nothing -> Nothing
--                  Just x -> f x

-- So that the function of (>=) is like a function that helps us sequence (处理) the parameter
-- -- before putting them into the target function. And the
-- m1 >>= \x1 ->
--   m2 >>= \x2 ->
--   ...
--     mn >>= \xn ->
--       f x1 x2 ... xn
-- Then the parameters for f are safe. And I do not need to deal with Nothing parameters.
-- It is COOL!
-- And they have a more readable version:
-- do x1 <- m1
--    x2 <- m2
--    ...
--    xn <- mn
--    f x1 x2 ... xn


{-
Monad list
-}
-- [] (list) is an instance of Monad.
-- instance Monad [] where
--  return x = [x]
--  (x:xs) >= f = f x ++ (xs >= f)

-- Apply monad []
-- do x <- [1, 2, 3]
--    show x
-- This is like a for loop according to list [1, 2, 3] because of Monad [] defination


{-
State Transformer
-}

type ST a = State -> (a -> State)
-- return :: a -> ST a
-- rerturn x = \s -> (a, s)
--  This means that when return a state value, just bind it with a inputed state.
--  And this is so COOL! Because the type a is generic, it means type of x could be any type.

-- (>=) :: ST a -> (a -> ST b) -> ST b
-- st >= f = \s -> let (x, t') = st t in f x t'
--  This means that a inputed state are supposed to be processed by function f.
--  Note the generic of State parameter.

-- Example: Global Counter

{-
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

-}
