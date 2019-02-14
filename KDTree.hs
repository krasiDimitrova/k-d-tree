module KDTree (
    KDTree,
    fromList,
    nearestNeighbor,
    searchInterval) where

import Data.List (sortBy)
import Data.Ord (comparing)

type VectorSelectors a b = [a -> b]

vector2D :: [(a, a) -> a]
vector2D = [x, y]
    where
        x (a, _) = a
        y (_, b) = b

vector3D :: [(a, a, a) -> a]
vector3D = [x, y, z]
  where
    x (a, _, _) = a
    y (_, b, _) = b
    z (_, _, c) = c

data Tree a = Node a (Tree a) (Tree a)
            | Empty
 
instance Show a => Show (Tree a) where
  show Empty                    = "Empty"
  show (Node value left right) =
    "(" ++ show value ++ " " ++ show left ++ " " ++ show right ++ ")" 
 
data KDTree a b = KDTree (VectorSelectors a b) (Tree a)
 
instance Show a => Show (KDTree a b) where
  show (KDTree _ tree) = "KDTree " ++ show tree
 
euclidDist :: Num b => VectorSelectors a b -> a -> a -> b
euclidDist ds point1 point2 = sum (map (^2)  (zipWith (-) point1' point2'))
  where
    point1' = map ($ point1) ds
    point2' = map ($ point2) ds
 
fromList :: Ord b => VectorSelectors a b -> [a] -> KDTree a b
fromList ds values = KDTree ds (makeTree (cycle ds) values)

makeTree :: Ord b => VectorSelectors a b -> [a] -> Tree a
makeTree _      []     = Empty
makeTree (d:ds) values =
    let
        sorted          = sortBy (comparing d) values
        (lower, higher) = splitAt ((length sorted) `div` 2) sorted
    in case higher of
        []          -> Empty
        median:rest -> Node median (makeTree ds lower) (makeTree ds rest)

nearestNeighbor :: (Ord b, Num b) => KDTree a b -> a -> Maybe a
nearestNeighbor (KDTree ds tree) sPoint = near ds tree
  where
    dist = euclidDist ds
    near _      Empty                    = Nothing
    near _      (Node point Empty Empty) = Just point
    near (d:ds) (Node point left right)  =
        let
          splitDist      = euclidDist (d:ds) sPoint point
          hyperPlaneDist = ((d sPoint) - (d point))^2
          bestLeft       = near ds left
          bestRight      = near ds right
          (maybeThisBest, maybeOtherBest) =
            if d sPoint < d point
            then (bestLeft, bestRight)
            else (bestRight, bestLeft)
        in case maybeThisBest of
            Nothing -> 
                case maybeOtherBest of
                    Nothing        -> Just point
                    Just otherBest ->
                        if euclidDist (d:ds) sPoint otherBest < splitDist
                        then maybeOtherBest
                        else Just point 
            Just thisBest ->
                let thisBestDist = euclidDist (d:ds) sPoint thisBest
                    best =
                        if splitDist < thisBestDist
                        then point
                        else thisBest
                    bestDist = euclidDist (d:ds) sPoint best
                in
                    if bestDist < hyperPlaneDist
                    then Just best
                    else
                        case maybeOtherBest of
                            Nothing        -> Just best
                            Just otherBest ->
                                if bestDist < euclidDist (d:ds) sPoint otherBest
                                then Just best
                                else maybeOtherBest


searchInterval :: Ord b => KDTree a b -> [(b,b)] -> [a]
searchInterval (KDTree ds t) inter = searchInterval' ds t inter

searchInterval' :: Ord b => VectorSelectors a b -> Tree a -> [(b,b)] -> [a]
searchInterval' _ Empty _ = []
searchInterval' ds t [] = kDTreeToList t
searchInterval' [] t _  = kDTreeToList t
searchInterval' (d:ds) (Node point l r) (x:xs)
    | d point == fst x && inInterval (d:ds) point (x:xs) = point:searchInterval' ds r xs
    | d point == snd x && inInterval (d:ds) point (x:xs) = point:searchInterval' ds l xs
    | inInterval (d:ds) point (x:xs) = [point] ++ searchInterval' ds l xs ++ searchInterval' ds r xs
    | d point <= fst x                           = searchInterval' ds r xs
    | d point >= snd x                           = searchInterval' ds l xs
    | otherwise = []

inInterval :: Ord b => VectorSelectors a b -> a -> [(b,b)]-> Bool
inInterval []     point _      = True
inInterval (d:ds) point (x:xs) = (d point >= fst x) && (d point <= snd x )&& (inInterval ds point xs)

kDTreeToList :: Tree a -> [a]
kDTreeToList Empty            = []
kDTreeToList (Node point l r) = [point] ++ kDTreeToList l ++ kDTreeToList r