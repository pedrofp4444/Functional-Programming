module Ficha9 where

import Data.List
import System.Random

-- Exercício 1

-- Alínea a)

bingo :: IO ()
bingo = bingoAux [1..90]

bingoAux :: [Int] -> IO ()
bingoAux [] = return ()
bingoAux prev_ns = do
    putStr "Prime ENTER para gerar um novo número."
    getChar
    random_index <- randomRIO (1, length prev_ns)
    let n = prev_ns !! (random_index - 1)
    print n
    bingoAux (delete n prev_ns)

-- Alínea b)

mastermind :: IO ()
mastermind = sequence (replicate 4 $ randomRIO (0,9))
    >>= playMastermind

playMastermind :: [Int] -> IO ()
playMastermind numbers = do
    guess <- map read . words <$> getLine :: IO [Int]
    if length guess /= 4 then
        putStrLn "Sequencia invalida!"
        >> playMastermind numbers
    else
        let (right_loc, wrong_loc) = 
                foldr (\i (right_loc, wrong_loc) -> 
                    if guess !! i == numbers !! i then
                        (right_loc + 1, wrong_loc)
                    else if guess !! i `elem` numbers then
                        (right_loc, wrong_loc + 1)
                    else
                        (right_loc, wrong_loc)
                ) (0,0) [0..3] in
        if right_loc == 4 then
            putStrLn "Parabens! Acertaste na sequencia!"
        else
            putStrLn (unlines [
                "Valores corretos: " ++ show right_loc,
                "Valores no local errado: " ++ show wrong_loc
            ])
            >> playMastermind numbers

