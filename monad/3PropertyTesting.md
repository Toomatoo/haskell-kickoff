# QuickCheck: Type-directed Property Testing

For testing, instead of going through test cases in order to see if the function
gets right results, we focus on properties that the results of function should
obey.

## Property

Property is mostly a logistic formula which test some property of the result in
order to see if the formula is true or false.

### ==>
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
