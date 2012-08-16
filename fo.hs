import Debug.Trace

data Sentence = Connective Sentence Connective Sentence
	| Quantifier Quantifier [Var] Sentence
	| Not Sentence
	| Atom Atom deriving (Eq, Read, Show)

data Atom = Predicate Predicate [Term]
	| Equal Term Term deriving (Eq, Read, Show)

data Term = Function Const [Term]
	| Const Const
	| Var Var deriving (Eq, Read, Show)

data Connective = If | Iff | And | Or deriving (Eq, Read, Show)

data Quantifier = ForAll | Exists deriving (Eq, Read, Show)

type Var = String
type Const = String
type Function = String
type Predicate = String

data Subst = Subst Var Const


--unify :: Atom -> Atom -> [Atom] -> Maybe Atom

--unifyVar :: Var -> Atom ->
