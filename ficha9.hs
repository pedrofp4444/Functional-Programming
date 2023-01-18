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

-- Exercício 2

data Aposta = Ap [Int] (Int,Int)

-- Alínea a)

valida :: Aposta -> Bool
valida (Ap nums@[n1,n2,n3,n4,n5] (e1,e2)) = 
    all (`elem` [1..50]) nums
    && nub nums == nums
    && e1 /= e2
    && e1 `elem` [1..12] && e2 `elem` [1..12]    
valida _ = False

-- Alínea b)

comuns :: Aposta -> Aposta -> (Int, Int)
comuns (Ap nums (e1,e2)) (Ap nums_chave (e1_c, e2_c)) = 
    (length (nums `intersect` nums_chave),
     length (filter (`elem` [e1_c, e2_c]) [e1, e2]))

-- Alínea c)

instance Eq Aposta where
    (==) a b = comuns a b == (5,2)

premio :: Aposta -> Aposta -> Maybe Int
premio ap chave =
    case comuns ap chave of 
        (5,e) -> Just (3 - e)
        (4,e) -> Just (6 - e)
        (3,2) -> Just 7
        (3,e) -> Just (10 - e)
        (2,2) -> Just 8
        (2,e) -> Just (13 - e)
        (1,2) -> Just 11
        _ -> Nothing

-- Alínea d)

leAposta :: IO Aposta
leAposta = do
    putStrLn "Introduz 5 números separados por um espaço:"
    nums <- map read . words <$> getLine :: IO [Int]
    putStrLn "Introduz as 2 estrelas separadas por um espaço:"
    estrelas <- map read . words <$> getLine :: IO [Int]
    if length estrelas /= 2 then
        putStrLn "Aposta invalida!"
        >> leAposta    
    else
        let ap = Ap nums ((\(a:b:_) -> (a,b)) estrelas) in
        if valida ap then
            return ap
        else
            putStrLn "Aposta invalida!"
            >> leAposta

joga :: Aposta -> IO ()
joga chave = do
    ap <- leAposta
    putStrLn $ "Premio: " ++ maybe "sem premio" show (premio ap chave)


-- Alínea e)

geraChave :: IO Aposta
geraChave = do
    nums <- foldr (\_ acc -> do
            prev_nums <- acc
            num <- geraNum (1,50) prev_nums
            return $ num : prev_nums
        ) (return []) [1..5]
    estrela1 <- geraNum (1,12) []
    estrela2 <- geraNum (1,12) [estrela1]
    return $ Ap nums (estrela1, estrela2)

geraNum :: (Int, Int) -> [Int] -> IO Int
geraNum range prev_nums = do
    n <- randomRIO range
    if n `elem` prev_nums then
        geraNum range prev_nums
    else
        return n

-- Alínea f)

main :: IO ()
main = do
    ch <- geraChave
    ciclo ch

menu :: IO String
menu = do
    putStrLn menutxt
    putStr "Opcao: "
    getLine
    where menutxt = unlines ["",
            "Apostar ........... 1",
            "Gerar nova chave .. 2",
            "",
            "Sair .............. 0"
            ]

ciclo :: Aposta -> IO ()
ciclo chave = do
    opcao <- menu
    case opcao of 
        "1" -> joga chave >> ciclo chave
        "2" -> geraChave >>= (\ch -> putStrLn "Nova chave gerada." >> ciclo ch)
        "0" -> return ()