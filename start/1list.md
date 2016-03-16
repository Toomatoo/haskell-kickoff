# List

Compare
```haskell
[3, 2] > [3, 1]
[3, 4, 1] == [3, 4, 1]
[3, 4, 1] > [3, 4]
```

functions of list
```haskell
head [1, 2, 3]
tail [1, 2, 3]
init [1, 2, 3]
last [1, 2, 3]
```

If the list is empty, the functions above maybe error
null [] -- check if the list is empty
```haskell
length [1, 2]
```
```haskell
[4, 8 .. 20]
```
```haskell
bloom xs = [if x < 10 then "Boom" else "Safe" | x <- xs, odd x]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' x = sum [1 | _ <- x]
```
