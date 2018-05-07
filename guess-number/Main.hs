module Main where

import System.Random
import Text.Read

minSecretValue = 0
maxSecretValue = 1000

type Guess = Int
type Secret = Int

evalGuess :: Guess -> Secret -> Ordering 
evalGuess = compare 

guessAttempt :: Int -> IO (Ordering)
guessAttempt secret = do
  putStrLn "New guess?"
  guessMay <- readMaybe <$> getLine
  case guessMay of
    Nothing -> do putStrLn "Bad input"; guessAttempt secret
    Just guess -> return $ evalGuess guess secret

getHint :: Ordering -> String 
getHint result = case result of
  LT -> "You are below"
  GT -> "You are above"
  EQ -> "You found the good value"

gameLoop :: Int -> Int -> IO ()
gameLoop secret attemptCount = do
  result <- guessAttempt secret
  putStrLn $ getHint result
  if result == EQ
    then putStrLn $ "Found in " ++ show attemptCount ++ " attempts" 
    else gameLoop secret (attemptCount + 1)  

main :: IO ()
main = do
  secret <- getStdRandom $ randomR (minSecretValue, maxSecretValue)
  putStrLn $ "You must find a number between " ++ show minSecretValue ++ " and " ++ show maxSecretValue 
  gameLoop secret attemptCount
  where attemptCount = 1 
