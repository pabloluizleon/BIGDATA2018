module Main where

{-
Exercício 01: Crie uma função ehTriangulo que determina se três lados x, y, z podem formar um triângulo.
-- Autor: Pablo L. Leon
-- Data: 10/03/2018
-}
ehTriangulo :: Double -> Double -> Double -> Bool
ehTriangulo x y z | (x + y) > z = True
                  | (x + z) > y = True
                  | (y + z) > x = True
                  | otherwise = False

{-
Exercício 02: Crie uma função tipoTriangulo que determina o tipo do triângulo formado pelos três lados x, y, z.
-- Autor: Pablo L. Leon
-- Data: 10/03/2018
-}
tipoTriangulo :: Double -> Double -> Double -> String
tipoTriangulo x y z | (x == y)&&(x == z) = "Tipo: Triangulo Equilatero"
                    | (x == y)||(x == z) = "Tipo: Triangulo Isosceles"
                    | otherwise = "Tipo: Triangulo Escaleno"

{-
Exercício 03: Implemente uma função que faz a multiplicação etíope entre dois números.
-- Autor: Pablo L. Leon
-- Data: 10/03/2018
-}

par :: Integer -> Bool
par a = (a `rem` 2 == 0)

multicacaoEtiope :: Integer -> Integer -> Integer
multicacaoEtiope 1 n = n
multicacaoEtiope m n
    | par m = multicacaoEtiope (m`div`2) (n*2)
    | otherwise = multicacaoEtiope (m `div` 2) (n*2) + n



{-
Exercício 04: Faça uma função que determine se um número é primo.
-- Autor: Pablo L. Leon
-- Data: 10/03/2018
-}
ehPrimo :: Integer -> Bool
ehPrimo 1 = False
ehPrimo 2 = True
ehPrimo x = ehPrimo2 (x) (x-1)

ehPrimo2 :: Integer -> Integer -> Bool
ehPrimo2 x 2 = nDivide x 2
ehPrimo2 x y = (nDivide x y) && (ehPrimo2 (x) (y-1))

nDivide :: Integer -> Integer -> Bool
nDivide a b = (a `rem` b /=0)

{-
Exercício 05: Faça uma função que calcule a soma dos dígitos de um número.
-- Autor: Pablo L. Leon
-- Data: 10/03/2018
-}
soma :: Integer -> Integer
soma 0 = 0
soma x = (x `mod` 10) + soma (x `div` 10)

{-
Exercício 06: Faça uma função que calcule a persistência aditiva de um número.
-- Autor: Pablo L. Leon
-- Data: 10/03/2018
-}
persistenciaAditiva :: Integer -> Integer
persistenciaAditiva x
    | (x == 0 || x == 1 || x == 2 || x == 3 || x == 4 || x == 5 || x == 6 || x == 7 || x == 8 || x == 9) = 0
    | otherwise = 1 + persistenciaAditiva (soma x)



{-
Exercício 07: Faça uma função que calcule o coeficiente binomial de (m,n).
-- Autor: Pablo L. Leon
-- Data: 10/03/2018
-}
coBinomial :: Integer -> Integer -> Integer
coBinomial x 0 = 1
coBinomial 0 y = 0
coBinomial x y =   (x `div` y) * (coBinomial (x-1) (y-1))


{-
Exercício 08: Faça uma função que calcule o elemento (i,j) do triângulo de pascal.
-- Autor: Pablo L. Leon
-- Data: 10/03/2018
-}
trianguloPascal :: Integer -> Integer -> Integer
trianguloPascal _ 0 = 1
trianguloPascal x 1 = x
trianguloPascal x y
    | x > y = (trianguloPascal (x-1) (y-1)) + (trianguloPascal (x-1) (y))
    | x == y = 1
    | otherwise = error "Não é possível calcular com esses coeficientes"

-- |'main' executa programa principal
main :: IO ()
main = do
    print ("Exercicio 1 - ehTriangulo")
    print (ehTriangulo 10 10 11)
    print ("Exercicio 2 - Tipo de Triangulo")
    print (tipoTriangulo 12 11 10)
    print ("Exercicio 3 - Multiplicacao Etiope")
    print (multicacaoEtiope 2 3)
    print ("Exercicio 4 - Se o numeor e primo")
    print (ehPrimo 14)
    print ("Exercicio 5 - Soma dos digitos de um numero")
    print (soma 10)
    print ("Exercicio 6 - Persistencia aditiva de um numero")
    print (persistenciaAditiva 11)
    print ("Exercicio 7 - Coeficiente binomial de (x,y)")
    print (coBinomial 2 5)
    print ("Exercicio 8 - Calculo do triangulo de pascal")
    print (trianguloPascal 3 3)
