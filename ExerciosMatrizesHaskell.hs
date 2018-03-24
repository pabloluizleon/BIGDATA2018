module Main where

{-
Exercício 01: Faça uma função que gere uma matriz identidade de tamanho n.
-- Autor: Pablo L. Leon
-- Data: 23/03/2018
-}

linha :: Int -> Int -> [Int]
linha 0 x = []
linha n x
    | n==x = 1:linha (n-1) x
    | otherwise = 0:linha (n-1) x

matriz :: Int -> Int -> [[Int]]
matriz n 1 = [linha n 1]
matriz n x = linha n x : matriz n (x-1)

geraMatriz :: Int -> [[Int]]
geraMatriz 0 = [[]]
geraMatriz n = matriz n n

{-
Exercício 02: Faça uma função que calcule a soma da diagonal principal de uma matriz.
-- Autor: Pablo L. Leon
-- Data: 23/03/2018
-}
somaDiagonalPrincipal :: [[Integer]] -> Integer
somaDiagonalPrincipal matriz = foldl (+) 0 [matriz !! x !! x | x <- [0..(length matriz - 1)]]

{-
Exercício 03: Faça uma função que calcule a soma da diagonal secundária de uma matriz.
-- Autor: Pablo L. Leon
-- Data: 23/03/2018
-}
somaDiagonalSecundaria :: [[Integer]] -> Integer
somaDiagonalSecundaria matriz = foldl (+) 0 [matriz !! x !! (length matriz - 1 - x) | x <- [0..(length matriz - 1)]]



-- |'main' executa programa principal
main :: IO ()
main = do
    print ("Exercicio 1 - gerar matriz")
    print (geraMatriz 3)
    
    print ("Exercicio 2 - Calcula soma diagonal principal da matriz")
    print(somaDiagonalPrincipal [[1,0,0],[0,1,0],[0,0,1]])
    print(somaDiagonalPrincipal [[1,0,0],[0,2,0],[0,0,3]])

    print ("Exercicio 3 - Calcula soma diagonal secundaria da matriz")
    print(somaDiagonalSecundaria [[0,0,1],[0,1,0],[0,0,1]])
    print(somaDiagonalSecundaria [[0,0,1],[0,2,0],[0,0,3]])
