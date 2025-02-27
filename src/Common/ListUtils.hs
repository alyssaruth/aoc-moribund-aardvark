{-# LANGUAGE TupleSections #-}

module Common.ListUtils where

import           Data.List (group, sort, tails)
import qualified Data.Map  as M
import qualified Data.Set  as S
import Combinatorics (tuples)

freqs :: (Ord k, Num a) => [k] -> M.Map k a
freqs xs = M.fromListWith (+) (map (, 1) xs)

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = snd . minimum . map (\xs -> (length xs, head xs)) . group . sort

mapIf :: (b -> Bool) -> (b -> b) -> [b] -> [b]
mapIf p f = map (\x -> if p x then f x else x)

window2 :: [a] -> [(a, a)]
window2 l@(_:xs) = zip l xs
window2 _        = []

window3 :: [a] -> [(a, a, a)]
window3 l@(_:y:xs) = zip3 l (y : xs) xs
window3 _          = []

windowN :: Int -> [a] -> [[a]]
windowN n xs = filter ((== n) . length) $ map (take n) $ tails xs

allSets :: (Ord a) => [a] -> [S.Set a]
allSets xs = gen
  where
    sizes = [0 .. length xs]
    gen = map S.fromList $ concatMap (`tuples` xs) sizes

singleton :: a -> [a]
singleton x = [x]

flexibleRange :: (Integral a) => a -> a -> [a]
flexibleRange a b
  | b >= a = [a .. b]
  | otherwise = [a,(a - 1) .. b]

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil pred l@(x:xs)
  | pred x = l
  | otherwise = dropUntil pred xs


associateBy :: Ord b => (a -> b) -> [a] -> M.Map b [a]
associateBy fn list = M.fromListWith (++) [(fn item, [item]) | item <- list]