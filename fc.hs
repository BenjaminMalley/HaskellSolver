module FC (
	Horn(..),
	KB,
	agenda, inferred, count, fc
) where

import qualified Data.Map as Map
import Data.List (find)

data Horn = Horn { head' :: String, body :: Maybe [String] } deriving (Read, Eq, Show, Ord)
type KB = [Horn]
type Agenda = KB
type Query = String
type Count = Map.Map Horn Int
type Inferred = Map.Map Horn Bool

agenda :: KB -> KB
agenda = filter $ \x -> body x == Nothing

count :: KB -> Map.Map Horn Int 
count = foldl count' Map.empty where
	count' map clause = case body clause of
		Nothing -> Map.insert clause 0 map
		Just l -> Map.insert clause (length l) map

inferred :: KB -> Map.Map Horn Bool
inferred = foldl add Map.empty where
	add map clause = Map.insert (Horn (head' clause) Nothing) False $ case body clause of
		Nothing -> map
		Just l -> foldl (\m c -> Map.insert (Horn c Nothing) False m) map l

decrementClause :: Map.Map Horn Int -> Horn -> Map.Map Horn Int
decrementClause map clause = case Map.lookup clause map of 
	Just n -> Map.insert clause (n - 1) map
	Nothing -> map --not expecting to get here
	
inClause :: Horn -> Horn -> Bool
inClause s clause = case body clause of
		Nothing -> False
		Just b -> case find (== (head' s)) b of
			Nothing -> False
			Just _ -> True

		
fc :: KB -> Agenda -> Query -> Count -> Inferred -> Bool
fc _ [] _ _ _ = False
fc kb (p:ps) q count inf
	| (head' p)==q = True
	| otherwise = case Map.lookup p inf of
		Just True -> fc kb ps q count inf
		Just False -> fc kb ps' q count' inf' where
			inf' = Map.insert p True inf
			count' = foldl decrementClause count $ filter (inClause p) kb
			ps' = (map (\x -> Horn (head' x) Nothing) $ Map.keys $ Map.filter (== 0) count') ++ ps
		Nothing -> fc kb ps q count inf

kb = [Horn "Q" $ Just ["P"], Horn "P" $ Just ["L", "M"], Horn "M" $ Just ["B", "L"], Horn "L" $ Just ["A", "P"], Horn "L" $ Just ["A", "B"], Horn "A" Nothing, Horn "B" Nothing]

main = putStrLn $ show $ fc kb (agenda kb) "L" (count kb) (inferred kb)

