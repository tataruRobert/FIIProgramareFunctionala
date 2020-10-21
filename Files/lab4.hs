data ArbNode a = Leaf
               | Node a (ArbNode a ) (ArbNode a)


dst :: (Int, Int) -> (Int, Int) -> Double
dst p1 p2 = undefined


data FourTuple a b c d = Fst a
                        | Snd b
                        | Trd c
                        | Frt d
                        | Empty
                        deriving Show

nth :: Int -> (a,b,c,d) -> FourTuple a b c d
nth 0 (x, _, _, _) = Fst x
nth 1 ( _, x, _, _) = Snd x
nth 2 ( _, _, x, _) = Trd x
nth 3 ( _, _, _, x) = Frt x
nth _ _ = Empty


-- Ex 1 :
ex1 :: (Int -> Int) -> Int -> Int -> Int



