module FO where

import Data.List (find)

data Sentence = Connective Sentence Connective Sentence
	| Quantifier Quantifier [Var] Sentence
	| Not Sentence
	| Atom Atom deriving (Eq, Read, Show)

data Atom = Predicate Predicate [Term]
	| Equal Term Term deriving (Eq, Read, Show)

data Term = Function Function [Term] 
	| Const Const
	| Var Var deriving (Eq, Read, Show)

data Connective = If | Iff | And | Or deriving (Eq, Read, Show)

data Quantifier = ForAll | Exists deriving (Eq, Read, Show)

type Var = String
type Const = String
type Function = String
type Predicate = String

-- Although a Subst is technically a (Var, Const), the unification algorithm
-- is sufficient to ensure this.
type Subst = (Term, Term)

findSubst :: Term -> Maybe [Subst] -> Maybe Term
findSubst _ Nothing = Nothing
findSubst x (Just subs) = case find (\(v, _) -> x == v) subs of
	Nothing -> Nothing
	(Just (_, c)) -> Just c

unifyList :: [Term] -> [Term] -> Maybe [Subst] -> Maybe [Subst]
unifyList [] [] theta = theta
unifyList [] (y:ys) _ = Nothing --return failure for lists of unequal length
unifyList (x:xs) [] _ = Nothing
unifyList (x:xs) (y:ys) theta = unifyList xs ys $ unify x y theta

-- Algorithm has some modifications from the one in AIMA for typing reasons:
-- 1. Handling of lists has been moved off into unifyList.
-- 2. For Functions x & y, unify checks their equality in place, instead of recurring.
-- The other approach would have been to make Function = Function Const [Term] but this seemed like cheating.
unify :: Term -> Term -> Maybe [Subst] -> Maybe [Subst]
unify _ _ Nothing = Nothing
unify x y theta | x == y = theta
unify x@(Var _) y theta = unifyVar x y theta
unify x y@(Var _) theta = unifyVar y x theta
unify x@(Function f args) y@(Function f' args') theta = unifyList args args' $ if f == f' then theta else Nothing
unify _ _ _ = Nothing

unifyVar :: Term -> Term -> Maybe [Subst] -> Maybe [Subst]
unifyVar v x theta@(Just subs) = case findSubst v theta of
	(Just val) -> unify val x theta
	Nothing -> case findSubst x theta of
		(Just val') -> unify v val' theta
		Nothing -> if occurCheck v x then Nothing else Just $ (v, x) : subs

occurCheck :: Term -> Term -> Bool
occurCheck v (Function f (arg:args)) = checkList v args where
	checkList v [] = True
	checkList v (x:xs) = (occurCheck v x) && (checkList v xs)
occurCheck v x = v == x	

-- from AIMA, pg 277
-- returns Just [(Var "x", Const "Jane")]
main = putStrLn $ show $ unify (Function "Knows" [Const "John", Var "x"]) (Function "Knows" [Const "John", Const "Jane"]) (Just [])