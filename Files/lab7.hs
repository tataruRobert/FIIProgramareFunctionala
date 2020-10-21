type Id = String


data Term = Var Id
          | App Term Term
          | Lambda Id Term
          deriving (Eq)

instance Show Term where
    show (Var x) = x
    show (App t1 t2) = "(" ++ (show t1) ++ " " ++ (show t2) ++ ")" 
    show (Lambda id t) = "\\" ++ id ++ "." ++ (show t)


x = Var "x"
y = Var "y"
z = Var "z"


r011  = Lambda "x" (Lambda "y" x)

subst :: Id -> Term -> Term -> Term
subst id term (Var id') | id == id' = term
                        | otherwise = (Var id')

subst id term (App term1 term2) = App (subst id term term1) (subst id term term2)
subst id term (Lambda id' term') 
         | id == id' = (Lambda id' term')
         | True = (Lambda id' (subst id term term'))

r021 = subst "x" y z
r023 = subst "y" z (App x y)
--r025 = subst ""


remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd : t1) | id == hd = remove id t1
                    | otherwise = hd : (remove id t1)


free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = (free term1) ++ (free term2) 
free (Lambda id term) = remove id (free term)


vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = (vars term1) ++ (vars term2) 
vars (Lambda id term) = [id] ++ (vars term) 


-- fresh' :: [Id] -> Int -> Id
-- fresh' ids index | name `elem` ids = fresh' ids (index + 1)
--                  | otherwise = name
--                  where name = "n" ++ show index

-- fresh :: [Id] -> Id
-- fresh ids = fresh' 0



-- casubst :: Id -> Term -> Term -> [Id] -> Term
-- casubst id term (Var id') _ | id == id' = term
--                             | otherwise = (Var id')
-- casubst id term (App term1 term2) avoid = App (casubst id term term1 avoid)
-- casubst id term (Lambda id' term') avoid 
--          | id == id' = 
--          | id' `elem` (free term) =
--                   let id'' = fresh avoid in         
--          | otherwise = Lambda id' ...
