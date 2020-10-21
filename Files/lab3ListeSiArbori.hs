data Nod = Const Int Nod Nod
		| Vid deriving Show

arboreBinar :: Nod -> Bool
arboreBinar Vid = True
arboreBinar (Const a Vid Vid) = True
arboreBinar (Const a (Const b n1 n2) (Const c n3 n4)) = if ((b < a) && (a > c)) then ((arboreBinar (Const b n1 n2)) && (arboreBinar (Const c n3 n4))) else False
arboreBinar (Const a (Const b n1 n2) Vid) = if (b < a) then arboreBinar (Const b n1 n2) else False
arboreBinar (Const a Vid (Const c n3 n4)) = if (a > c) then arboreBinar (Const c n3 n4) else False

cautareArbore :: Int -> Nod -> Bool
cautareArbore a Vid = False
cautareArbore a (Const b n1 n2) = if (a == b) then True else if a < b then cautareArbore a n1 else cautareArbore a n2

stergeNod :: Int -> Nod -> Nod
stergeNod a (Const b Vid Vid) = if (a == b) then Vid else (Const b Vid Vid)
stergeNod a (Const b n1 Vid) = if (a == b) then n1 else if a < b then (Const b Vid (stergeNod a n1)) else (Const b n1 Vid)
stergeNod a (Const b Vid n1) = if (a == b) then n1 else if a < b then (Const b Vid n1) else Const b (stergeNod a n1) Vid
stergeNod a (Const b n1 n2) = if (a == b) then (Const (findMin n2) n1 (stergeNod (findMin n2) n2 )) else if a < b then (Const b (stergeNod a n1) n2) else (Const b n1 (stergeNod a n2))

findMin :: Nod -> Int
findMin (Const a Vid _) = a 
findMin (Const a n1 _) = findMin n1

adaugaNod :: Int -> Nod -> Nod
adaugaNod a Vid = Const a Vid Vid
adaugaNod a (Const b n1 n2) = if (a > b) then Const b n1 (adaugaNod a n2) else Const b (adaugaNod a n1) n2

data Lista = Cons Int Lista | Vida deriving Show

cautareLista :: Int -> Lista -> Bool
cautareLista a (Cons b l) = if (a == b) then True else cautareLista a l 
cautareLista a Vida = False

addToListEnd :: Int -> Lista -> Lista
addToListEnd a (Cons b l) = Cons b (addToListEnd a l)
addToListEnd a Vida = Cons a Vida

addToListHead :: Int -> Lista -> Lista
addToListHead a (Cons b l) = Cons a (Cons b l)

addToList :: Int -> Int -> Lista -> Lista
addToList a b (Cons c l) = if (b == 0) then Cons a (Cons c l) else Cons c (addToList a (b-1) l)
addToList a b Vida = Cons a Vida

maxLista :: Lista -> Int
maxLista Vida = 0
maxLista (Cons c l) = max c (maxLista l)
