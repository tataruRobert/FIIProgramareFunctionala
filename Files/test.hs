f :: Int -> Int
f x = x + 1

g :: Int -> Int
g x = x + 10



suma :: Int -> (Int -> Int)
suma x y = x + y 



suma3 :: Int -> (Int -> (Int -> Int))
suma3 x y z = x + y + z


f3 :: Int -> Int
f3 = div 10

f4 :: (Int -> Int) -> Int
f4 func = func 42

data Lista a = Vida
           | Cons a (Lista a)
            deriving Show

numarEl :: Lista a -> Int
numarEl Vida = 0
numarEl (Cons i t) = 1 + (numarEl t)


map' :: (a -> b) -> Lista a -> Lista b
map' _ Vida = Vida
map' f (Cons hd tl) = Cons (f hd) (map' f tl)

filter' :: (a -> Bool) -> Lista a -> Lista a
filter' p Vida = Vida
filter' p (Cons hd tl) = if p hd then Cons hd (filter' p tl) else filter' p tl

f' :: Int -> Bool
f' x | x `mod` 2 == 0 = True
     | x `mod` 2 == 1 = False


reduce :: (a -> a -> a) -> a -> Lista a -> a
reduce f x Vida = x
reduce f x (Cons hd tl) = reduce f (f x hd) tl

cmp :: (a -> b) -> ( b -> c ) -> a -> c
cmp f g x = g (f x)

sieu :: [a] -> (a -> b) -> [b]
sieu [] _ = []
sieu (x : xs) a = (a x):(sieu xs a)


filtru :: [a] -> (a -> Bool) -> [a]
filtru [] _ = []
filtru (x : xs) f = if (f x) == True then x : filtru xs f
                                   else (filtru xs f)

applyRange :: (Int -> Int) -> Int -> Int -> Int
applyRange f x y | x > y = 0
                 | otherwise = (f x) + (applyRange f (x + 1) y)

composition :: (b -> c) -> (a -> b) -> a -> c
composition f g x = f (g x)

data Eth a b = Lft a
              | Rgt b
                deriving (Show)



data List a = Empty
            | Node a (List a)
              deriving (Show)





foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ i Empty = i
foldLeft f i (Node val next) = f (foldLeft f i next) val

foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ i Empty = i
foldRight f i (Node val next) = f val (foldRight f i next)


composeList' :: [a -> a] -> a -> a
composeList' lst x = foldr (.) id lst $ x


search1FoldR :: (Eq a) => a -> [a] -> Bool
search1FoldR a xs = foldl (\x b -> (x == a) || b) False xs
