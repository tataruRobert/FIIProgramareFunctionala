data MobileDevice = Smartphone Culori
                  | Laptop Culori 
                  | Tablet Culori Int String
                  deriving (Show)

data Culori = Rosu
             | Albastru
             | Verde
             | Portocaliu
             deriving (Show)

desc :: MobileDevice -> String
desc (Smartphone _) = "Acesta este un laptop de culoare roz."
desc (Laptop _) = "Aceasta este o tableta mov."
desc (Tablet _ s _ ) = "Acesta este un telefon mobil."


getColor :: MobileDevice -> Culori
getColor (Smartphone c) = c
getColor (Laptop c) = c
getColor (Tablet c _ _ ) = c


data Nat = Zero
         | Succ Nat
          deriving (Show)


fromInt :: Int -> Nat
fromInt x | x <= 0 = Zero
          | otherwise = Succ (fromInt (x - 1))


toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ x) = 1 + (toInt x)

add :: Nat -> Nat -> Nat
add x Zero = x
add x (Succ y) = add (Succ x) y

data ErrorNat = Eroare
              | Val Nat
              deriving(Show)


minus :: Nat -> Nat -> ErrorNat
minus x Zero = Val  x
minus Zero x = Eroare
minus (Succ a) (Succ b) = a `minus` b  

multi :: Nat -> Nat -> Nat
multi x Zero = Zero
multi x (Succ Zero) =  x
multi x (Succ a) = (multi x a) `add` x


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

find :: Int -> Nod -> Bool
find _ Vid = False
find x (Const val left right)  
                    | x == val = True
                    | x < val = find x left 
                    | otherwise = find x right

adaugaNod :: Int -> Nod -> Nod
adaugaNod a Vid = Const a Vid Vid
adaugaNod a (Const b n1 n2) 
                        | (a > b) = Const b n1 (adaugaNod a n2) 
                        | otherwise Const b (adaugaNod a n1) n2 

stergeNod :: Int -> Nod -> Nod
stergeNod a (Const b Vid Vid) 
                          | (a == b) = Vid 
                          | otherwise =  (Const b Vid Vid)
stergeNod a (Const b n1 Vid) 
                          | (a == b) = n1 
                          |  a < b = (Const b (stergeNod a n1) Vid)
                          | otherwise (Const b n1 Vid)
stergeNod a (Const b Vid n1) 
                          | (a == b) =  n1 
                          | a < b = (Const b Vid n1) 
                          | otherwise =  Const b Vid (stergeNod a n1)
stergeNod a (Const b n1 n2) 
                          | (a == b) = (Const (findMin n2) n1 (stergeNod (findMin n2) n2 )) 
                          | a < b = (Const b (stergeNod a n1) n2) 
                          | otherwise = (Const b n1 (stergeNod a n2))

findMin :: Nod -> Int
findMin (Const a Vid _) = a 
findMin (Const a n1 _) = findMin n1







