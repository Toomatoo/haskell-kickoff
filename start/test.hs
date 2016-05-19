type State = Int
data ST a = S (State -> (a, State))

apply :: ST a -> State -> (a, State)
apply (S st) x = st x

instance Applicative ST where
instance Functor ST where

instance Monad ST where
  -- return :: a -> ST a
  return x   = S (\s -> (x, s))
  -- (>>=)  :: ST a -> (a -> ST b) -> ST b
  st >>= f   = S $ \s -> let (x, s') = apply st s in
                         apply (f x) s'

fresh :: ST Int
fresh = S (\n -> (n, n+1))

wtf = do
        st <- fresh
        fresh

baz mmx = do mx <- mmx
             x  <- mx
             return x


type A a= Int -> a

data Code = C1 Int | C2 String
                deriving (Show)
