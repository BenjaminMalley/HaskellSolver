data Sentence = Connective Sentence Connective Sentence
	| Quantifier Quantifier [Variable] Sentence
	| Not Sentence
	| Predicate String [Term]
	| Equal Term Term
	
data Term = Function String [Term]
	| Constant String
	| Variable Variable
	
type Variable = String
data Connective = If | Iff | And | Or
data Quantifier = ForAll | Exists

main = putStrLn "Hello"	