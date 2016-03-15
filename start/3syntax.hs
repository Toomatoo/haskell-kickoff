{-
Patterns
-}


--Define from up to down, otherwise, overlapping
lucky :: (Integral a) => a -> String
--lucky x = "Sorry"
lucky 1 = "One"
lucky 2 = "Two"
lucky x = "Sorry"

-- fab :: (Integer a) => a -> a IS WRONG, because Integer is not a typeclass
fab :: Int -> Int
fab 0 = 1
fab 1 = 1
-- remember to add parenthese, because of priority
fab x = fab (x-1) + fab (x-2)

-- play with tuple
first :: (a, b, c) -> a
first (a, _, _) = a


-- play with list
head' :: [a] -> a
head' [] = error "Empty"
head' (a:_) = a

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
-- Match the lists with one element
tell (x:[]) = "The list has one element: " ++ show x
-- Just match the lists with only two elements
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
-- Match with lists with two or more elements
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: [a] -> Int
length' [] = 0
-- the : operator type is a -> [a] -> [a], so take use of the type
-- the _ below denotes a, and _ in tell denotes [a]
length' (_:xs) = 1 + length' xs

-- Use a pattern and still have the reference to the whole pattern
firstLetter :: String -> String
firstLetter [] = "Empty String"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]





{-
Guards
-}


fat :: (RealFloat a) => a -> a -> String
fat weight height
    | bmi < skinny  = "Not fat"
    | bmi < normal  = "Bit more"
    | otherwise = "Go to gym"
    -- names need to be aligned, otherwise error from haskell
    where bmi = weight / height ^ 2
          --skinny = 10
          --normal = 20
          -- They can be rewritten like this
          (skinny, normal) = (10, 20)

{-
let <bindings> in <expression>
-}

surfacearea :: (RealFloat a) => a -> a -> a
surfacearea r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

-- > let square x = x*x in (square 2, square 3)
--   just aware of the defination of expression and binding

-- I can use let in a list and the scope of the bindings is limited
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]



{-
Case statement

case expression of pattern -> result
                   pattern -> result
                   pattern -> result
                   ...
-}

getStr :: [a] -> String
getStr xs = "The string is " ++ case xs of []  -> "empty."
                                           [x] -> "single element."
                                           (a:_) -> "long string"
