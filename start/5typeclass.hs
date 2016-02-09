-- Define a data
module Typeclass where
data TypeName = TypeConstructor1 | TypeConstructor2

-- Export the functions and types that we defined in a module:
-- module Shapes
-- ( Point(..)
-- , Shape(..)
-- , surface
-- , nudge
-- , baseCircle
-- , baseRect
-- ) where

-- Shape(..) means you can use all the constructors for Shape
-- Otherwise, users can only use some auxiliary functions to make Shapes (not constructors)
-- Not exporting the value constructors of a data types makes them more abstract in such a way that
-- we hide their implementation. Also, whoever uses our module can't pattern match against the value
-- constructors.



{-
    Record Syntax
    Then some functions are automatically created.
    e.g. firstName :: Person -> String
-}
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)




{-
The other two ways one may introduce types to Haskell programs are via the type and newtype
statements. type introduces a synonym for a type and uses the same data constructors. newtype
introduces a renaming of a type and requires you to provide new constructors.
-}

-- newtype is much similar with data, and newtype can always be replaced by data


{-
    Type Parameters
-}

data Tree a = Branch (Tree a) (Tree a) | Leaf a
-- This data type consists of one type parameter a and two kinds of data constructors.
-- The same defination methods are:
type ST a = State -> (a, State)
class Eq a where
    (==) :: a -> a -> Bool
    ...
instance Monad [] where
    return x = [x]





{-
    Type Class
-}
-- class  Eq a  where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

-- This is if a is an instance of class Eq, then a must have two functions here.

-- The typeclass definition also provides default implementations of each operation (in terms of the
-- other operation.) Like the example below:
-- class Eq a  where
--     (==)           :: a -> a -> Bool
--     (/=)           :: a -> a -> Bool
--
--     {- Default Implementations -}
--
--     x == y         = not (x /= y)
--     x /= y         = not (x == y)

{-
    Instance of Typeclass
-}
nameOf :: Person -> String
nameOf (Person fir las a h p f) = fir ++ las

-- For a data for my own, I can make it an instance of Eq for standard use
instance Eq Person where
    p1 == p2 = nameOf p1 == nameOf p2
