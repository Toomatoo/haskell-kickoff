# Type Class

## Define a data
```haskell
module Typeclass where
data TypeName = TypeConstructor1 | TypeConstructor2
```
Export the functions and types that we defined in a module:
```haskell
module Shapes
 ( Point(..)
 , Shape(..)
 , surface
 , nudge
 , baseCircle
 , baseRect
 ) where
```

Shape(..) means you can use all the constructors for Shape
Otherwise, users can only use some auxiliary functions to make Shapes (not constructors)
Not exporting the value constructors of a data types makes them more abstract in such a way that
we hide their implementation. Also, whoever uses our module can't pattern match against the value
constructors.

### Record Syntax
Then some functions are automatically created.
e.g. firstName :: Person -> String
```haskell
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)
```


The other two ways one may introduce types to Haskell programs are via the type and newtype
statements. type introduces a synonym for a type and uses the same data constructors. newtype
introduces a renaming of a type and requires you to provide new constructors.

newtype is much similar with data, and newtype can always be replaced by data



## Type Parameters

```haskell
data Tree a = Branch (Tree a) (Tree a) | Leaf a
```
This data type consists of one type parameter a and two kinds of data constructors.
The same defination methods are:
```haskell
type ST a = State -> (a, State)
```




## Type Class
**What is Typeclass?**

A collection of operations that must exist for underlying type.

```haskell
class  Eq a  where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

This is if a is an instance of class Eq, then a must have two functions here.
So that

The typeclass definition also provides default implementations of each operation
(in terms of the other operation.) Like the example below:

```haskell
class Eq a  where
     (==)           :: a -> a -> Bool
     (/=)           :: a -> a -> Bool

     {- Default Implementations -}

     x == y         = not (x /= y)
     x /= y         = not (x == y)
```

More complex, we can define a type class with other type class
```haskell
class (Eq a, Show a) => Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
```
It means if a wants to be a instance of `Num`, it must first be instances of
`Eq` and `Show`. Then you can make `a` as a instance of `Num` by implementing
the asked functions.




## Instance of Typeclass

```haskell
nameOf :: Person -> String
nameOf (Person fir las a h p f) = fir ++ las
```
For a data for my own, I can make it an instance of Eq for standard use
```haskell
instance Eq Person where
    p1 == p2 = nameOf p1 == nameOf p2
```

When you define a type as a instance of some type class, then you can call the
function with the variables of this type. Like `(==) p1 p2`.

See if you define lots of types as instances of `Eq`, then you can call `(==)`
with polymorphism. I mean `1 == 2`, `a == b`. It is very convenient as the
example of `JSon` the professor has mentioned. Every kinds of data may be called
with `toJson`, at the same time you get a `JSon` object.

Last thing for instance of type class is that when you finished a implementation
for a type class, you can move forward and define a complex combination to be a
instance of the same type class. For example, if you have define type `a` to be a
instance of `JSson`, then
```haskell
instance (JSon a) => JSon (String, a) where
    toJSon = ...
```
which means, if you've finished `instance JSon a`, it does not means that you do
not need to define `instance JSon (String, a)`. Before that, the system cannot
recognize `toJSon ("name", object of a)`. The situation happens in build-in `Eq`,
you can try `(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 1, 1, 1, 1, 1)
== (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 1, 1, 1, 1, 1)`. There is no such instance
of the long tuple.

## A Type of Type: kind
```
> :kind Type
> :kind (->)
> * -> * -> *
```
This means (->) accept two Types and output a Type
Like for Int, the kind is \*, it means it does not need any input to generate
-- a new type.
