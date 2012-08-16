module FO where

import Debug.Trace
import Data.List (find)

data Sentence = Connective Sentence Connective Sentence
	| Quantifier Quantifier [Var] Sentence
	| Not Sentence
	| Atom Atom deriving (Eq, Read, Show)

data Atom = Predicate Predicate [Term]
	| Equal Term Term deriving (Eq, Read, Show)

data Term = Function Term [Term] --for typing; should be Function Function [Term]
	| Const Const
	| Var Var deriving (Eq, Read, Show)

data Connective = If | Iff | And | Or deriving (Eq, Read, Show)

data Quantifier = ForAll | Exists deriving (Eq, Read, Show)

type Var = String
type Const = String
type Function = String
type Predicate = String

data Subst = Subst Term Term --temporary hack to avoid typing issues; should be Subst Var Const

findSubst :: Term -> Maybe [Subst] -> Maybe Term
findSubst _ Nothing = Nothing
findSubst x (Just subs) = case find (\(Subst v _) -> x == v) subs of
	Nothing -> Nothing
	(Just (Subst _ c)) -> Just c

unifyList :: [Term] -> [Term] -> Maybe [Subst] -> Maybe [Subst]
unifyList [] [] theta = theta
unifyList [] (y:ys) _ = Nothing --return failure for lists of unequal length
unifyList (x:xs) [] _ = Nothing --confidence that this is right = 50%
unifyList (x:xs) (y:ys) theta = unifyList xs ys $ unify x y theta

unify :: Term -> Term -> Maybe [Subst] -> Maybe [Subst]
unify _ _ Nothing = Nothing
unify x y theta | x == y = theta
unify x@(Var _) y theta = unifyVar x y theta
unify x y@(Var _) theta = unifyVar y x theta
unify x@(Function f args) y@(Function f' args') theta = unifyList args args' $ unify f f' theta
unify _ _ _ = Nothing

unifyVar :: Term -> Term -> Maybe [Subst] -> Maybe [Subst]
unifyVar v x theta@(Just subs) = case findSubst v theta of
	(Just val) -> unify val x theta
	Nothing -> case findSubst x theta of
		(Just val') -> unify v val' theta
		Nothing -> if occurCheck v x then Nothing else Just $ (Subst v x) : subs

occurCheck :: Term -> Term -> Bool
occurCheck v (Var _) = False
occurCheck v (Const _) = False
occurCheck v (Function f args) = elem v args

