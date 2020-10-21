data Nat = Zero 
         | Succ Nat
         

instance Show Nat where
    show Zero = "o"
    show (Succ x) = "s(" ++ show x ++ ")"

instance Eq Nat where
    (==) Zero Zero = True
    (==) Zero (Succ x) = False
    (==) (Succ x) Zero = False
    (==) (Succ x) (Succ y) = (==) x y   

instance Ord Nat where
    (<=) Zero Zero = True
    (<=) Zero (Succ x) = True
    (<=) (Succ a) Zero = False
    (<=) (Succ a) (Succ b) = (<=) a b

{-
data Arb = Leaf
         | Node Int Arb Arb


instance Show Arb where
    show Leaf = "()"
    show (Node x y z) = "(" ++ show(x) ++  show (y) ++ show (z) ++ ")"  
--(2(3()())(4()()))
arbore :: Arb
arbore = Node 2 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf)
-}

data Arb a = Leaf
           | Node a (Arb a) (Arb a)

instance (Show a) => Show (Arb a) where
        show Leaf = "()"
        show (Node x y z) = "(" ++ show(x) ++  show (y) ++ show (z) ++ ")" 

instance (Eq a) => Eq (Arb a) where
    (==) Leaf Leaf = True
    (==) Leaf (Node _ _ _) = False
    (==) (Node a b c) (Node e d f) | a == e && (==) b d && (==) c f
                                   
    (==) (Node _ _ _) (Node _ _ _) = False


arbore :: Arb Float
arbore = Node 2.4 Leaf Leaf

