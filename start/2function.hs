doubleMe x = x + x

doubleSmaller x = if x > 100
                    then x * 2
                    else x

--1. First letter of function name cannot be capital
doubleSmaller' x = if x > 100 then x*2 else x

-- : operator is putting at the begin
putBegin x = x : [1, 2]

-- !! operator find the xth element in the list
getNElem l x = l !! x
