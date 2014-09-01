module Main where

import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Control.Monad (liftM)

splitHead :: [a] -> Maybe (a, [a])
splitHead (x:xs) = Just (x, xs)
splitHead _ = Nothing

firstInt :: (String, a) -> Maybe (Int, a)
firstInt (nstr, xs) = case mn of
	Just n -> Just (n, xs)
	Nothing -> Nothing
	where
		mn = readMaybe nstr :: Maybe Int

moreThan (n, xs)
	| length xs >= n = Just (n, xs)
	| otherwise = Nothing

divide :: Eq a => (Int, [a]) -> [[a]]
divide (n, people) = combinationsOf n people

combinationsOf :: Eq a => Int -> [a] -> [[a]]
combinationsOf 0 xs = []
combinationsOf 1 xs = map (:[]) xs
combinationsOf n xs =	let singleExtension = \x -> (map (x:) (combinationsOf 1 (xs `without` x)))
						in concatMap singleExtension xs
--combinationsOf n xs = concat $ map (\x -> x: ( combinationsOf (n-1) (xs `without` x))) xs

without :: (Eq a) => [a] -> a -> [a]
without xs x = filter (/= x) xs

testPeople = ["Cecilie", "Teodor", "Kristina", "Erik", "Ingerid"]
testArgs = ["2", "Cecilie", "Teodor", "Kristina", "Erik", "Ingerid"]

validMaybe args = (splitHead args) >>= firstInt >>= moreThan

prettyPrintPairs :: [[String]] -> String
prettyPrintPairs = (intercalate "\n") . (map (intercalate " og "))

prettyPrintPairsMaybe :: Maybe [[String]] -> String
prettyPrintPairsMaybe (Just xs) = prettyPrintPairs xs
prettyPrintPairsMaybe Nothing = "Invalid input"

main = do
	args <- getArgs
	let validArgs = validMaybe args
	putStrLn . prettyPrintPairsMaybe $ liftM divide validArgs
