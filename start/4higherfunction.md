# Higher Order Function

## Curried Functions


```haskell
compareWithHundred :: (Num a, Ord a) => a -> Ordering
-- compareWithHundred x = compare x 100
-- rewrite it as a curried function
compareWithHundred = compare 100
```





## Function as parameter

```haskell
addOne :: Int -> Int
addOne x = x + 1
-- the parenthese in the type is mandatory
doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)
```

```haskell
ghci> doTwice (+3) 10
16
ghci> doTwice (++ " HAHA") "HEY"
"HEY HAHA HAHA"
ghci> doTwice ("HAHA " ++) "HEY"
"HAHA HAHA HEY"
ghci> doTwice (multThree 2 2) 9
144
ghci> doTwice (3:) [1]
[3,3,1]
```




# Anonymous Function

```haskell
f :: Int -> Int -> Int -> Int
f = \x1 x2 x3 -> x1 + x2 + x3
```


## Infix and Secion


Haskell allows you to use any function as an infix operator, simply by
wrapping it inside backticks.
```haskell
2 `div` 3

`div` 3 :: Int -> Double
```


## Computation Pattern


### map
transfer a list of elements to a list of other elements

### fold

**foldr**

```haskell
foldr op base [x1,x2,...,xn]
  == {- unfold -}
     x1 `op` (foldr op base [x2,...,xn])
  == {- unfold -}
     x1 `op` (x2 `op` (foldr op base [...,xn]))
  == {- unfold -}
     x1 `op` (x2 `op` (... `op` (xn `op` base)))
op :: x -> out -> out
```
The type of output is the same as base case
The point of foldr is that op accepts one element of original list and the
output of foldr. So that the computation is right-combined.

```haskell
foldl op base [x1, x2, ..., xn]
op :: base -> x -> base
```
The type of output is the same as base case
The point of foldl is that op accepts base (beforehead output) and a element
of original list. So that the computation is left-combinged

fold is actually a implementation of recursion. First you define a base case:
when is the base case (e.g. fold takes [] as base case), and what is the
result of the base case (e.g. fold takes a custermized result for base case).
Then, you define a recursive step: also when is the recursive step happened
(e.g. for fold, take out the first element if possible) and what is the result
of the resursion (e.g. fold defines 'op').

My point to say something about the mechanism of fold is that you can define
your own fold on your own data structure. It is a step of Abstraction, then
you are free to define more actions with some basic actions and the fold you
defined. For example, in a tree structure

```haskell
treeFold op b (Leaf x)   = b x
treeFold op b (Node l r) = (treeFold op b l)
                           `op`
                           (treeFold op b r)
```
Then you can correspond everything in this fold to the things described above.
Like, if you want to get the number of leaves in the tree, then you can
```haskell
size   = treeFold (+) (const 1)
```

The same thing happens in 'map'. Everything is abstraction!

```haskell
listLen = foldr (\_ n -> n + 1) 0
```


## Some key words/functions

**filter**
```haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let leftPart = quicksort (filter (<  x) xs)
        rightPart= quicksort (filter (>= x) xs)
    in  leftPart ++ [x] ++ rightPart
```
**takeWhile**
takeWhile take a predicte and a list. It goes from the beginning of the list and returns its
elements while the predict holds true. Once the predict doesn't hold true, it stops.
Ex. sum (takeWhile (<10000) (filter odd (map (^2) [1..])))


**$**
```haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```
$ has a low precedence, so that x will be computed first to be the parameter of f
