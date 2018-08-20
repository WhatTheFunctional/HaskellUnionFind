--UnionFind.hs
--Copyright Laurence Emms 2018
--Union find algorithm implementation

--Input: A list of pairs, where each pair (p, q) represents a connection between p and q.
--Note: Connections are an equivalence relation (reflexive, symmetric, and transitive).
--      Connected componenets form an equivalence class w.r.t. the connection operator.
--Goal: Write a program which filters out pairs which are in the same equivalence class
--      given previous pairs. i.e. Only print out pairs which connect two components
--      to one-another.

import Data.Array

type Component = Int

class UnionFind a where
    find :: a -> Int -> Component
    union :: a -> Int -> Int -> a
    cmp :: a -> Int -> Int -> Bool
    count :: a -> Int

-- QuickFind

data QuickFind = QuickFind Int (Array Int Int) deriving (Show, Eq)

initQuickFind :: Int -> QuickFind
initQuickFind n = QuickFind n (listArray (0, n - 1) [0..(n - 1)])

unionQuickFind :: Int -> QuickFind -> Int -> Int -> [(Int, Int)] -> QuickFind
unionQuickFind 0 (QuickFind n array) i j updates = QuickFind (n - (length updates)) (array // updates)
unionQuickFind x (QuickFind n array) i j updates = if (array ! x) == j
                                                   then unionQuickFind (x - 1) (QuickFind n array) i j ((x, i) : updates)
                                                   else unionQuickFind (x - 1) (QuickFind n array) i j updates

instance UnionFind QuickFind where
    find (QuickFind n array) i = array ! i
    union (QuickFind n array) i j
        | i == j = (QuickFind n array)
        | otherwise = unionQuickFind (n - 1) (QuickFind n array) (find (QuickFind n array) i) (find (QuickFind n array) j) []
    cmp (QuickFind n array) i j = (find (QuickFind n array) i) == (find (QuickFind n array) j)
    count (QuickFind n array) = n

-- QuickUnion

data QuickUnion = QuickUnion Int (Array Int Int) deriving (Show, Eq)

initQuickUnion :: Int -> QuickUnion
initQuickUnion n = QuickUnion n (listArray (0, n - 1) [0..(n - 1)])

instance UnionFind QuickUnion where
    find (QuickUnion n array) i
        | i == (array ! i) = i
        | otherwise = find (QuickUnion n array) (array ! i)
    union (QuickUnion n array) i j
        | i == j = QuickUnion n array
        | otherwise = QuickUnion (n - 1) (array // [(find (QuickUnion n array) j, find (QuickUnion n array) i)])
    cmp (QuickUnion n array) i j = (find (QuickUnion n array) i) == (find (QuickUnion n array) j)
    count (QuickUnion n array) = n

-- WeightedQuickUnion

data WeightedQuickUnion = WeightedQuickUnion Int (Array Int Int) (Array Int Int) deriving (Show, Eq)

initWeightedQuickUnion :: Int -> WeightedQuickUnion
initWeightedQuickUnion n = WeightedQuickUnion n (listArray (0, n - 1) [0..(n - 1)]) (listArray (0, n - 1) (take n (repeat 1)))

instance UnionFind WeightedQuickUnion where
    find (WeightedQuickUnion n array sizes) i
        | i == (array ! i) = i
        | otherwise = find (WeightedQuickUnion n array sizes) (array ! i)
    union (WeightedQuickUnion n array sizes) i j
        | i == j = WeightedQuickUnion n array sizes
        | otherwise = let sizeI = sizes ! i
                          sizeJ = sizes ! j
                          sizeSum = sizeI + sizeJ
                      in if sizeI < sizeJ
                         then WeightedQuickUnion (n - 1) (array // [(find (WeightedQuickUnion n array sizes) i, find (WeightedQuickUnion n array sizes) j)]) (sizes // [(j, sizeSum)])
                         else WeightedQuickUnion (n - 1) (array // [(find (WeightedQuickUnion n array sizes) j, find (WeightedQuickUnion n array sizes) i)]) (sizes // [(i, sizeSum)])
    cmp (WeightedQuickUnion n array sizes) i j = (find (WeightedQuickUnion n array sizes) i) == (find (WeightedQuickUnion n array sizes) j)
    count (WeightedQuickUnion n array sizes) = n

main :: IO ()
main = do putStrLn "Union Find"
          putStrLn "\n"

          let quickFindTest = initQuickFind 10
          putStrLn "Quick Find:"
          putStrLn (show quickFindTest)
          putStrLn ("Component at index 5: " ++ (show (find quickFindTest 5)))
          putStrLn ("Compare 2 and 5: " ++ (show (cmp quickFindTest 2 5)))
          putStrLn "Now joining 2 and 5."
          let quickFind1 = union quickFindTest 2 5
          putStrLn (show quickFind1)
          putStrLn ("Component at index 5: " ++ (show (find quickFind1 5)))
          putStrLn ("Compare 2 and 5: " ++ (show (cmp quickFind1 2 5)))
          putStrLn "Now joining 5 and 3."
          let quickFind2 = union quickFind1 5 3
          putStrLn (show quickFind2)
          putStrLn ("Component at index 5: " ++ (show (find quickFind2 5)))
          putStrLn ("Compare 2 and 3: " ++ (show (cmp quickFind2 2 3)))
          putStrLn "\n"

          let quickUnionTest = initQuickUnion 10
          putStrLn "Quick Union:"
          putStrLn (show quickUnionTest)
          putStrLn ("Component at index 5: " ++ (show (find quickUnionTest 5)))
          putStrLn ("Compare 2 and 5: " ++ (show (cmp quickUnionTest 2 5)))
          putStrLn "Now joining 2 and 5."
          let quickUnion1 = union quickUnionTest 2 5
          putStrLn (show quickUnion1)
          putStrLn ("Component at index 5: " ++ (show (find quickUnion1 5)))
          putStrLn ("Compare 2 and 5: " ++ (show (cmp quickUnion1 2 5)))
          putStrLn "Now joining 5 and 3."
          let quickUnion2 = union quickUnion1 5 3
          putStrLn (show quickUnion2)
          putStrLn ("Component at index 5: " ++ (show (find quickUnion2 5)))
          putStrLn ("Compare 2 and 3: " ++ (show (cmp quickUnion2 2 3)))
          putStrLn "\n"

          let weightedQuickUnionTest = initWeightedQuickUnion 10
          putStrLn "Weighted Quick Union:"
          putStrLn (show weightedQuickUnionTest)
          putStrLn ("Component at index 5: " ++ (show (find weightedQuickUnionTest 5)))
          putStrLn ("Compare 2 and 5: " ++ (show (cmp weightedQuickUnionTest 2 5)))
          putStrLn "Now joining 2 and 5."
          let weightedQuickUnion1 = union weightedQuickUnionTest 2 5
          putStrLn (show weightedQuickUnion1)
          putStrLn ("Component at index 5: " ++ (show (find weightedQuickUnion1 5)))
          putStrLn ("Compare 2 and 5: " ++ (show (cmp weightedQuickUnion1 2 5)))
          putStrLn "Now joining 5 and 3."
          let weightedQuickUnion2 = union weightedQuickUnion1 5 3
          putStrLn (show weightedQuickUnion2)
          putStrLn ("Component at index 5: " ++ (show (find weightedQuickUnion2 5)))
          putStrLn ("Compare 2 and 3: " ++ (show (cmp weightedQuickUnion2 2 3)))
          putStrLn "\n"

