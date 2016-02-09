main :: IO ()
main = return ()
{-
Curried Functions
-}

compareWithHundred :: (Num a, Ord a) => a -> Ordering
-- compareWithHundred x = compare x 100
-- rewrite it as a curried function
compareWithHundred = compare 100





{-
Function as parameter
-}

addOne :: Int -> Int
addOne x = x + 1
-- the parenthese in the type is mandatory
doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)

{-
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
-}


{-
Infix Functions
-}
isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A'..'Z'])

-- '->' is right associative by default

-- filter function
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let leftPart = quicksort (filter (<  x) xs)
        rightPart= quicksort (filter (>= x) xs)
    in  leftPart ++ [x] ++ rightPart

-- takeWhile function
-- takeWhile take a predicte and a list. It goes from the beginning of the list and returns its
-- elements while the predict holds true. Once the predict doesn't hold true, it stops.
-- Ex. sum (takeWhile (<10000) (filter odd (map (^2) [1..])))


-- '$' keyword
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
-- $ has a low precedence, so that x will be computed first to be the parameter of f
