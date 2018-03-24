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
soma :: [[Int]] -> Int
soma [[]] = 0
soma x = sum (somaDiagonal x)

somaDiagonal :: [[Int]] -> [Int]
somaDiagonal x = zipWith (!!) x [0..]


{-
Exercício 03: Faça uma função que calcule a soma da diagonal secundária de uma matriz.
-- Autor: Pablo L. Leon
-- Data: 23/03/2018
-}
somasec :: [[Int]] -> Int
somasec [[]] = 0
somasec x = sum (somadiagonalsecundaria x)

somadiagonalsecundaria :: [[Int]] -> [Int]
somadiagonalsecundaria x = zipWith (!!) x (reverse [0.. (length (x !! 0)-1)])



-- |'main' executa programa principal
main :: IO ()
main = do
    print ("Exercicio 1 - gerar matriz")
    print (geraMatriz 3)
    
    print ("Exercicio 2 - Calcula soma diagonal principal da matriz")
    let matriz1 = [[]]
    print (soma matriz1)
    let matriz2 = [[3,3], [3,3], [3,3]]
    print(soma matriz2)

    print ("Exercicio 3 - Calcula soma diagonal secundaria da matriz")
    let matriz1 = [[]]
    print (soma matriz1)
    let matriz2 = [[3,2], [3,2], [3,2]]
    print(soma matriz2)
