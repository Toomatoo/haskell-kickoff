# QuickCheck: Type-directed Property Testing

For testing, instead of going through test cases in order to see if the function
gets right results, we focus on properties that the results of function should
obey.

## Property

A QuickCheck property is a function whose output is boolean.

Property is mostly a logistic formula which test some property of the result in
order to see if the formula is true or false.

For example,
```haskell
prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs ++ ys) == reverse xs ++ reverse ys
```
The type signature for the property is not polymorphic signature. This is
because QC uses the type to generate random inputs, and hence is restricted to
monomorphic properties.

```haskell
squickCheckN   :: (Testable p) => Int -> p -> IO ()
quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }
```


### `==>` Conditional Property

If we do not want to test on some specific test cases, we can use `==>` to
generate a `Property` which can be used by `quickCheck`.

Like, we not want to check on empty list for the function.
```haskell
prop_qsort_nn_min    :: [Int] -> Property
prop_qsort_nn_min xs =
  not (null xs) ==> head (qsort xs) == minimum xs
```

### Choose which property to test

We need to find some reference implementation that we can trust. For example,
we can test the result of a mySort function to standard `sort` function. But
it may take risk, because when you think the behavior of both of functions are
same, they can be not. Under this circumstance, it does not mean we cannot use
`sort` to check our results. We can use `==>` to exclude the cases that can lead
to different behavior of these two functions.

For example, `mySort` is going to eliminate all the duplicates, but `sort` will
not. Under this situation, we can still generate a `Property` for cases that do
not have duplicates. The code is Like
```haskell
prop_qsort_distinct_sort :: [Int] -> Property
prop_qsort_distinct_sort xs =
  (isDistinct xs) ==> (qsort xs == sort xs)
```
See we can define a custermized pre-check `isDistinct` which return a boolean.
Then we dive into the real check.

### Why use `==>`

```haskell
isOrdered xs ==> isOrdered (insert x xs)
not (isOrdered xs) || isOrdered (insert x xs)
```
For the first statement, if the precondition does not pass, the test case will
be discarded by the `quickCheck`. But for the second statement, if the
precondition does not pass which means (`not (isOrdered xs)` is true), then the
overall check will be true. It is actually not what we want to check. So the
precondition here is useful.

However, we can take use of the second format.
```haskell
prop_insert_ordered_vacuous' :: Int -> [Int] -> Property
prop_insert_ordered_vacuous' x xs =
  -- collect (length xs) $
  classify (isOrdered xs) "ord" $
  classify (not (isOrdered xs)) "not-ord" $
  not (isOrdered xs) || isOrdered (insert x xs)
```
run with `classify :: Bool -> String -> Property -> Property`, we can classify
different test cases, and the result will show like:
```text
ghci> quickCheck prop_insert_ordered_vacuous'
+++ OK, passed 100 tests:
 9% 1, ord
 2% 0, ord
 2% 2, ord
 5% 8, not-ord
 4% 7, not-ord
 4% 5, not-ord
 ...
```

## Generating Data: Gen

A Haskell term that generates a (random value) of type a has the type `Gen a`
which is defined as
```haskell
newtype Gen a = MkGen { unGen :: StdGen -> Int -> a }
```
the term is a function that takes as input a random number generator `StdGen`
and a seed `Int` and returns an a value. And we can make `Gen` as a `Monad`.

Then you can use `sample` to print some generated values
```haskell
sample :: Show a => Gen a -> IO ()
```
### Arbitrary

There is type class `Arbitrary` which helps to define `Gen`.

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

If type `a` is a instance of `Arbitrary`, then we can use `arbitrary` to generate
random value of `a`.

### Generator Combinators

```haskell
-- choose between two value of type `a`
choose :: (System.Random.Random a) => (a, a) -> Gen a
ghci > sample $ choose (0, 3)

-- choose from a list value of type `a`
elements :: [a] -> Gen a
ghci> sample $ elements [10, 20..100]

-- choose from a list generator of type `Gen a`
oneof :: [Gen a] -> Gen a
ghci> sample $ oneof [elements [10,20,30], choose (0,3)]

-- choose a generator according to some distribution
frequency :: [(Int, Gen a)] -> Gen a

```

Implementation of `oneof`
```haskell
oneof :: [Gen a] -> Gen a
oneof [] = error "QuickCheck.oneof used with empty list"
oneof gs = choose (0,length gs - 1) >>= (gs !!)
```
choose a index first, then find the generator.

### Case Study: Generate ordered Lists

First, we can write in a normal logic to generate a List
```haskell
genList1 ::  (Arbitrary a) => Gen [a]
genList1 = liftM2 (:) arbitrary genList1

liftM2 f mx my = do x <- m1
                    y <- m2
                    return $ f x y
```
But it generates a infinite list.

Second, we make it stop somewhere
```haskell
genList2 ::  (Arbitrary a) => Gen [a]
genList2 = oneof [ return []
                 , liftM2 (:) arbitrary genList2]
```
It is not bad, but we can make it less likely to generate a empty list.

Third,
```haskell
genList3 ::  (Arbitrary a) => Gen [a]
genList3 = frequency [ (1, return [])
                     , (7, liftM2 (:) arbitrary genList2) ]
```

Finally, we can sort it with `Function <$>` `<$>` is same as `fmap`
```haskell
genOrdList :: (Ord a, Arbitrary a) => Gen [a]
genOrdList = sort <$> genList3

<$> :: (Functor f) => (a -> b) -> f a -> f b
```

If you want to check a custom generator we can use `forAll` combinator.
```haskell
forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property

-- Generally, the `prop` is `Bool`
```
it accept a custom `Gen a` and a function that first accepts a value of `a`
(generated from the custom `Gen a`) and returns a property.

To test our `genOrdList` and `isOrdered`
```haskell
quickCheckN 1000 (forAll genOrdList isOrdered)
-- `forAll genOrdList isOrdered` gives us a `Property`
```
