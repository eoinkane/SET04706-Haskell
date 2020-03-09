{-
READ ME: Change the name of this file to YOURSTUDENTNUMBER.hs. For example, if your
student number were 123456789, then you would rename the file 123456789.hs.

REPLACE the function definitions for each of the questions. 
The names of the functions correspond to the names given in the document cwk19handout.pdf. 

DO NOT CHANGE THE TYPE SIGNATURES, OR THE ORDER OF THE ARGUMENTS!

You may add as many extra or helper functions as you wish, but do not make any "import" statements.
-}

-- QUESTION 1: Sets

-- Question 1) a)

bigUnion :: (Eq a) => [[a]] -> [a]
bigUnion [] = error "Empty Array"
bigUnion xss = bigUnionDeDuplication (bigUnionHelper xss)

bigUnionAddAtEnd :: (Eq a) => a -> [a] -> [a]
bigUnionAddAtEnd x [] = [x]
bigUnionAddAtEnd x (y:ys) = y : bigUnionAddAtEnd x (ys)


bigUnionAppend :: (Eq a) => [a] -> [a] -> [a]
bigUnionAppend [] ys = ys
bigUnionAppend (x:xs) ys = bigUnionAppend (xs) (bigUnionAddAtEnd x ys)


bigUnionHelper :: (Eq a) => [[a]] -> [a]
bigUnionHelper [] = []
bigUnionHelper (x:xs) = bigUnionAppend (bigUnionHelper xs) x

bigUnionDeDuplication :: (Eq a) => [a] -> [a]
bigUnionDeDuplication [] = []
bigUnionDeDuplication (x: xs)
    | elem x xs = bigUnionDeDuplication xs
    | otherwise = (x:bigUnionDeDuplication(xs))

-- Question 1) b)
bigIntersection :: (Eq a) => [[a]] -> [a]
bigIntersection [] = error "Empty Array"
bigIntersection xss = bigIntersectionHelperNew xss

containsBoolean :: (Eq a) => a -> [a] -> Bool 
containsBoolean _ [] = False
containsBoolean y xs = elem y (xs) 

containsForEachNew :: (Eq a) => [a] -> [a] -> [a]
containsForEachNew [] _ = []
containsForEachNew (x:xs) ys  
    | containsBoolean x ys == True = (x: containsForEachNew xs ys)
    | otherwise = containsForEachNew xs ys

bigIntersectionHelperNew :: (Eq a) => [[a]] -> [a]
bigIntersectionHelperNew [] = []
bigIntersectionHelperNew (x:xss) 
    | length xss == 1 = containsForEachNew x (head xss) 
    | otherwise = containsForEachNew x ( bigIntersectionHelperNew xss )

-- Question 1) c)
-- TODO Finish this question

howManySetsContain :: (Eq a) => [[a]] -> [(a,Int)]
howManySetsContain xss = error "You've not tried to write howManySetsContain yet"

nextListContainCount :: (Eq a) => a -> [a] -> Int
nextListContainCount _ [] = 0
nextListContainCount x (y:ys)
    | x == y = 1 
    | otherwise = nextListContainCount x ys

howManyListsContainHelper :: (Eq a) => a -> [[a]] -> Int
howManyListsContainHelper x [] = 0
howManyListsContainHelper x (subList:listOfLists) = (nextListContainCount x subList) + (howManyListsContainHelper x listOfLists)

howManySubListsContainHelper :: (Eq a) => [a] -> [[a]] -> [(a, Int)]
howManySubListsContainHelper _ [] = []
howManySubListsContainHelper [] _ = []
howManySubListsContainHelper (x:xs) listOfLists = ((x,(howManyListsContainHelper x listOfLists)): (howManySubListsContainHelper xs listOfLists))

iterateOverListItemExists :: (Eq a) => a -> [(a, Int)] -> Bool
iterateOverListItemExists _ [] = False
iterateOverListItemExists value (x:xs)
    | fst(x) == value = True
    | otherwise = iterateOverListItemExists value xs

iterateOverSubListItemExists :: (Eq a) => a -> [[(a, Int)]] -> Bool
iterateOverSubListItemExists _ [] = False
iterateOverSubListItemExists x (subList:listOfLists)
    | (iterateOverListItemExists x subList) == True = True
    | otherwise = iterateOverSubListItemExists x listOfLists

iterateOverSubListsHelper :: (Eq a) => [[a]] -> [[(a, Int)]]
iterateOverSubListsHelper [] = [] 
iterateOverSubListsHelper (x:xs) = ((howManySubListsContainHelper x (x:xs)): iterateOverSubListsHelper xs)

-- TODO - Build This Function
-- Convert the result of iterateOverSubListsHelper from [[(a,Int)]] to [(a,Int)]
iterateOverSubListsHelperAppend :: (Eq a) => [[(a, Int)]] -> [(a,Int)]

-- TODO - Build This Function
-- Dedupe the result of iterateOverSubListsHelperAppend to not have any occurences of a pair with the same fst()
iterateOverSubListsHelperDeDupe :: (Eq a) => [(a, Int)] -> [(a,Int)]


-- TEST SET FOR Q1
{-
Your functions should have the following behaviour:
bigUnion [[1,2,3],[3,4,5],[2,4,6,8]] = [1,2,3,4,5,6,8]
bigUnion ["list a", "list b"] = "list ab"

bigIntersection [[1,2,3],[3,4,5],[2,4,6,8]] = []
bigIntersection ["list a", "list b"] = "list "

howManySetsContain [[1,2,3],[3,4,5],[2,4,6,8]] = [(1,1),(2,2),(3,2),(4,2),(5,1),(6,1),(8,1)]
howManySetsContain ["list 1", "list 2"] = [('l',2),('i',2),('s',2),('t',2),(' ',2),('1',1),('2',1)]
 
THE ORDER OF ELEMENTS IN THE RESULTS OF THESE FUNCTIONS IS NOT IMPORTANT.
-}



-- QUESTION 2: Functions and relations

oneHop :: (Eq a) => a -> [(a,a)] -> [a]
oneHop y xs = error "You've not tried to write oneHop yet"

nextSteps :: (Eq a) => [a] -> [(a,a)] -> [[a]]
nextSteps ps xs = error "You've not tried to write nextSteps yet"

allPathsFrom :: (Eq a) => a -> [(a,a)] -> [[a]]
allPathsFrom x ys = error "You've not tried to write allPathsFrom yet"

shortestCycle :: (Eq a) => a -> [(a,a)] -> Maybe [a]
shortestCycle x ys = error "You've not tried to write shortestCycle yet"

-- TEST SET FOR Q2
{-
Your functions should have the following behaviour:

oneHop 3 [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = [2,4,1]
oneHop 1 [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = [3,4]

DO NOT WORRY ABOUT THE ORDER OF THE ELEMENTS IN THE RETURN LIST

nextSteps [1,3] [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = [[1,3,2],[1,3,4],[1,3,1]]
nextSteps [3,4] [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = []

DO NOT WORRY ABOUT THE ORDER OF THE ELEMENTS IN THE RETURN LIST (i.e. THE ORDER THE LISTS APPEAR IN THE LIST OF LISTS)

allPathsFrom 3 [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = [[3,2],[3,4],[3,1],[3,2,3],[3,1,3],[3,1,4]]
allPathsFrom 1 [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = [[1,3],[1,4],[1,3,2],[1,3,1],[1,3,4],[1,3,2,3]]

DO NOT WORRY ABOUT THE ORDER OF THE ELEMENTS IN THE RETURN LIST (i.e. THE ORDER THE LISTS APPEAR IN THE LIST OF LISTS)

shortestCycle 1 [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = Just [1,3,1]
shortestCycle 4 [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = Nothing


-}



-- QUESTION 3: Primes

lastPrime :: Int -> Int
lastPrime n
    | n <= 99 = error "You've not tried to write lastPrime for 2 digit numbers"
    | n <= 9999 = error "You've not tried to write lastPrime for 4 digit numbers"
    | n <= 99999 = error "You've not tried to write lastPrime for 5 digit numbers"
    | n <= 999999 = error "You've not tried to write lastPrime for 6 digit numbers"
    | otherwise = error "You've not tried to write lastPrime for big numbers yet"

primeFactorisation :: Int -> [Int]
primeFactorisation n
    | n <= 99 = error "You've not tried to write primeFactorisation for 2 digit numbers"
    | n <= 9999 = error "You've not tried to write primeFactorisation for 4 digit numbers"
    | n <= 99999 = error "You've not tried to write primeFactorisation for 5 digit numbers"
    | n <= 999999 = error "You've not tried to write primeFactorisation for 6 digit numbers"
    | otherwise = error "You've not tried to write primeFactorisation for big numbers yet"

{- 
Leave the error messages in place if you do not want to attempt the parts for the input size. You should remove the guards up to the point you want to attempt. For example, if you were confident of anything up to five digits, the function would look like:

primeFactorisation n
    | n <= 99999 = whatever_your_calculation_is
    | n <= 999999 = error "..."
    | otherwise = error "..."

 -}




-- TEST SET FOR Q3
{-
Your functions should have the following behaviour:
lastPrime 73 = 71
lastPrime 64 = 61
primeFactorisation 75 = [3,5,5]
primeFactorisation 64 = [2,2,2,2,2,2]
-}




-- QUESTION 4: RSA

eTotient :: Int -> Int
eTotient n = error "You've not tried to write eTotient yet"

encode :: Int -> Int -> Int -> Int -> Maybe Int
encode p q m e = error "You've not tried to write encode yet"

-- TEST SET FOR Q4
{-
Your functions should have the following behaviour:
eTotient 54 = 18
eTotient 73 = 72
encode 37 23 29 5 = Just 347
encode 99 18 108 45 = Nothing
encode 37 17 23 48 = Nothing
-}

