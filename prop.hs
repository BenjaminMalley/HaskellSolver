import Data.List (nub, delete, find)

--http://johantibell.com/files/stanford-2011/performance.html#(18)
--data Pair = Pair {-# UNPACK #-} Bool {-# UNPACK #-} Sentence

data Sentence = And Sentence Sentence
	| Or Sentence Sentence
	| If Sentence Sentence
	| Iff Sentence Sentence
	| Not Sentence
	| T
	| F
	| Sym String deriving (Eq, Show)

--puts a sentence into conjunctive normal form (Ands of Ors).	
cnf :: Sentence -> Sentence
cnf (Iff a b) = cnf $ And (cnf (If a b)) (cnf (If b a))
cnf (If a b) = cnf $ Or (Not (cnf a)) (cnf b)
cnf (Not (Not a)) = cnf a
cnf (Not (And a b)) = cnf $ Or (Not (cnf a)) (Not (cnf b))
cnf (Not (Or a b)) = And (Not (cnf a)) (Not (cnf b))
cnf (Or (And b c) a) = cnf $ And (Or a b) (Or a c)
cnf (Or a (And b c)) = cnf $ And (Or a b) (Or a c)
cnf (And a b) = And (cnf a) (cnf b)
cnf (Or a b) = Or (cnf a) (cnf b) 
cnf a = a

conjuncts :: [Sentence] -> Sentence -> [Sentence]
conjuncts xs (And a b) = conjuncts (conjuncts xs b) a
conjuncts xs a = a : xs

disjuncts :: [Sentence] -> Sentence -> [Sentence]
disjuncts xs (Or a b) = disjuncts (disjuncts xs b) a
disjuncts xs a = a : xs

--expects clauses of the form (Not a) or a; i.e., does not work with nested Nots
comp :: Sentence -> Sentence -> Bool
comp (Not a) b = a==b
comp a (Not b) = a==b
comp _ _ = False

--we just concatenate the lists and call a helper function on the resulting big list
--resolve uses nub which is O(n^2); it might be better to use a hash map or set
--resolve a b = nub $ resolve' $ a ++ b where
	--resolve' [] = []
	--resolve' (x:xs) = x' ++ resolve' xs' where
		--mask = map (\e -> not $ comp x e) xs
		--xs' = map snd $ filter fst $ zipWith (,) mask xs
		--x' = if foldl (&&) True mask then [x] else []
		
resolve :: [Sentence] -> [Sentence] -> [[Sentence]]
resolve xs ys = resolve' [] xs ys where
	resolve' :: [Sentence] -> [Sentence] -> [Sentence] -> [[Sentence]]
	resolve' _ x [] = []
	resolve' _ [] y = []
	resolve' xs' (x:xs) ys = case findComplement x ys of
		Nothing -> resolve' (x:xs') xs ys --resolution rule is not applied; move onto next
		Just y' -> (xs' ++ (delete y' ys) ++ xs) : resolve' (x:xs') xs ys
		
findComplement :: Sentence -> [Sentence] -> Maybe Sentence
findComplement x [] = Nothing
findComplement x (y:ys)
	| x `comp` y = Just y
	| otherwise = findComplement x ys

resolution :: Sentence -> Sentence -> Bool
resolution kb a = loop' [] clauses' where
	prep' x = map (disjuncts []) $ conjuncts [] $ cnf x
	clauses' = nub $ (prep' kb) ++ (prep' $ Not a)
	loop' ns (x:[]) = if foldl (&&) True $ map (\x -> elem x clauses') (nub ns) then False
		else loop' [] (nub $ ns ++ clauses')
	loop' ns (x:y:xs) = let r' = resolve x y in
		if r' == [] then True else loop' (nub $ ns ++ r') (y:xs)
			
--Example from pg.216 of AIMA; resolves to True
main = putStrLn $ show $ resolution (And (Iff (Sym "B11") (Or (Sym ("P12")) (Sym ("P21")))) (Not (Sym "B11"))) (Not (Sym "P12"))
