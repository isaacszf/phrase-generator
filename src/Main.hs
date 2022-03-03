module Main where

import Data.List (group, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (compare)
import Phrases (linkedPhrases)
import System.Random (Random (randomR), newStdGen)

phrases n1 n2 c
  | n1 /= n2 = phs
  | otherwise =
    filter
      ( \p ->
          sortDesc
            (map fst (frequency (splitOn " " p)))
            < [2]
      )
      phs
  where
    sortDesc :: (Ord a) => [a] -> [a]
    sortDesc = sortBy (flip compare)

    phs = Phrases.linkedPhrases n1 n2 c

    frequency list = map (\l -> (length l, head l)) (group (sort list))

genRandomWord seed l =
  getRandomWord l
  where
    getRandomWord l = l !! (fst (randomR (1, length l - 1) seed) :: Int)

main :: IO ()
main = do
  -- Random generated seeds
  s1 <- newStdGen
  s2 <- newStdGen
  s3 <- newStdGen
  s4 <- newStdGen

  -- Reading File
  p <- readFile "src/utils/personas.txt"
  c <- readFile "src/utils/places.txt"

  -- Printing phrase
  print
    ( genRandomWord
        s1
        ( phrases
            (genRandomWord s2 (genPersonas p))
            (genRandomWord s3 (genPersonas p))
            (genRandomWord s4 (genPersonas c))
        )
    )
  where
    -- Parsing File
    genPersonas path = reverse $ splitOn "\n" path