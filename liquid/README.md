# Liquid Haskell

## Well-Typed Programs Can Go wrong

1. Divide by zero
2. Missing Keys
3. Segmentation fault
4. Heart bleed

## Refinement Type

**Refinement Types = Types + Predicates**

The refinement type definition is
```haskell
{-@ type Nat = {n:Int | 0 <= n} @-}

{-@ nat :: Nat @-}
nat = [0, 1, 2]
```

**Verify Properties**

Via SMT based Predicate Subtyping.


## Pre-condition

`impossible` never called at run-time.

because no value satisfies `false`.

```haskell
{-@ impossible :: {v:_ | false} -> a @-}
impossible msg = error msg
```

For example, we want to make sure that `safeDiv` will not divide zero.

```haskell
{-@ safeDiv :: Int -> {n:Int | n /= 0} -> Int   @-}
safeDiv _ 0 = impossible "divide-by-zero"
safeDiv x n = x `div` n
```

## Which Properties to Check

### size

You need to verify the size of input and output.

### Element

You need to verify if the elements are on the right track.

**`measure`**

One input and corresponding output.

**`inline`**

One line of checking code, no recursion.

`measure` and `inline` can only call `measure` and `inline` functions.

### Other Special Property: e.g. sorted

If we want to define a ordered pair data, then
```haskell
{-@ data OrdPair = OP { opX :: Int, opY :: {n:Int | opX <= n}  } @-}

okPair  = OP 2 4  -- legal
badPair = OP 4 2  -- illegal
```

You need to specify the type, not other things.
