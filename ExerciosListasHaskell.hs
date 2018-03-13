module Main where

{-
Exercício 01: Crie uma função divisivel20 x que retorna verdadeiro se x for divisível por todos os números de 1 a 20.
-- Autor: Pablo L. Leon
-- Data: 11/03/2018
-}
divisivel20 :: Int -> Bool
divisivel20 x = divisivel x [1..20]

divisivel :: Int -> [Int] -> Bool
divisivel _ [] = True
divisivel x y = (x `rem` (head y) == 0) && divisivel x (tail y)



{-
Exercício 02: Crie uma função projectEuler5 que retorna o primeiro número natural que retorna True para a função do exercício anterior. Pense em como reduzir o custo computacional.
-- Autor: Pablo L. Leon
-- Data: 11/03/2018
-}
divisivelPlus :: Int -> [Int] -> Bool
divisivelPlus _ [] = True
divisivelPlus x y
    | (x `rem` (head y) /= 0) = False
    | otherwise = divisivelPlus x (tail y)

encontra :: Int -> Int
encontra x
    | (divisivel20 x == True) = x
    | otherwise = encontra (x+20)

projectEuler5 ::  Int
projectEuler5 = encontra 20


{-
Exercício 03: Crie a lista de números de Fibonacci utilizando uma função geradora.
-- Autor: Pablo L. Leon
-- Data: 11/03/2018
-}
lista = 1 : 2 : prox lista
    where
        prox (x : t@(y:_)) = (x+y) : prox t


{-
Exercício 04: Utilizando a lista anterior, calcule a soma dos números de Fibonacci pares dos valores que não excedem 4.000.000. (Project Euler 2)
-- Autor: Pablo L. Leon
-- Data: 10/03/2018
-}
soma :: Integer
soma = somaPares (take 4000000 lista)

somaPares :: [Integer] -> Integer
somaPares [x]
    | x `rem` 2 == 0 = x
    | otherwise = 0
somaPares (x:xs)
    | x `rem` 2 == 0 = x + somaPares xs
    | otherwise = somaPares xs

{-
Exercício 05: Faça uma função para calcular o produto escalar entre dois vetores.
-- Autor: Pablo L. Leon
-- Data: 11/03/2018
-}
calcProduto :: [Int] -> [Int] -> Int
calcProduto a b = sum (zipWith (*) a b)


{-
Exercício 06: Crie a função collatz x que retorna x/2
, se x for par e (3x+1)
 se for ímpar.
-- Autor: Pablo L. Leon
-- Data: 11/03/2018
-}
collatz :: Integer -> Integer
collatz x
    | x == 1 = 1
    | (x `rem` 2 ==0) = x `div` 2
    | otherwise = ((3*x)+1)

{-
Exercício 07: Implemente uma função collatzLen x que retorna o tamanho da lista formada pela aplicação repetida de collatz sobre o valor x até que essa chegue no número 1.
-- Autor: Pablo L. Leon
-- Data: 11/03/2018
-}
collatzLen :: Integer -> Integer
collatzLen x
    | x == 1 = 1
    | otherwise = 1 + collatzLen (collatz x)

{-
Exercício 08: Encontre o número x entre 1 e 1.000.000 que tem a maior sequência de Collatz. (Project Euler 14)
-- Autor: Pablo L. Leon
-- Data: 11/03/2018
-}
calculaCollatz :: [Integer] -> [Integer]
calculaCollatz [] = []
calculaCollatz (x:xs) = collatzLen (x) : calculaCollatz (xs)

maiorSequenciaCollatz = maximum (aplicaCollatz [1..1000000])



-- |'main' executa programa principal
main :: IO ()
main = do
    print ("Exercicio 1 - eh divisivel")
    print (divisivel20 14)
    print ("Exercicio 2 - projectEuler5 - retorna o primeiro numero natural que retorna True para a funcao do exercicio anterior.")
    print (projectEuler5)
    print ("Exercicio 3 - Sequencia de Fibonacci")
    print (take 10 lista)
    print ("Exercicio 4 - Numeros de Fibonacci pares dos valores que nao excedem 4.000.000")
    print (soma)
    print ("Exercicio 5 - Produto escalar de dois vetores")
    print (calcProduto [1, 7] [2,-3])
    print ("Exercicio 6 - Funcao collatz")
    print (collatz 10)
    print ("Exercicio 7 - Funcao collatzLen")
    print (collatzLen 20)
    print ("Exercicio 8 - Maior sequencia de collatz.")
    print (maiorSequenciaCollatz)
   
   
