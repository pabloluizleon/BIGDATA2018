{\rtf1\ansi\ansicpg1252\cocoartf1561\cocoasubrtf200
{\fonttbl\f0\fnil\fcharset0 Menlo-Regular;}
{\colortbl;\red255\green255\blue255;\red27\green31\blue34;}
{\*\expandedcolortbl;;\cssrgb\c14118\c16078\c18039;}
\paperw11900\paperh16840\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\deftab720
\pard\pardeftab720\sl320\partightenfactor0

\f0\fs28 \cf2 \expnd0\expndtw0\kerning0
\outl0\strokewidth0 \strokec2 module Main where\
\
\{-\
-- Exercicio 2\
-- Fa\'e7a uma fun\'e7\'e3o mult3  x que retorne True caso a entrada seja m\'faltiplo de 3 e False caso contr\'e1rio.\
-- Autor: Pablo L. Leon\
-- Data: 03/03/2018\
-\}\
mult3 :: Integer -> Bool\
mult3 var = if mod var 3 == 0 then True else False\
\
\{-\
-- Exercicio 3\
-- Fa\'e7a uma fun\'e7\'e3o mult5  x que retorne True caso a entrada seja m\'faltiplo de 5 e False caso contr\'e1rio.\
-- Autor: Pablo L. Leon\
-- Data: 03/03/2018\
-\}\
mult5 :: Integer -> Bool\
mult5 var = if mod var 5 == 0 then True else False\
\
\{-\
-- Exercicio 4\
-- Fa\'e7a uma fun\'e7\'e3o mult35 x que retorne True caso a entrada seja m\'faltiplo de 3 e 5 e False caso contr\'e1rio.\
-- Autor: Pablo L. Leon\
-- Data: 03/03/2018\
-\}\
mult35 :: Integer -> Bool\
mult35 var = if mod var 5 == 0 && mod var 3 == 0 then True else False\
\
\{-\
-- Exercicio 5\
-- Fa\'e7a um programa que retorne True caso a entrada seja menor que -1 ou (maior que 1 E m\'faltiplo de 2), e False caso contr\'e1rio.\
-- Autor: Pablo L. Leon\
-- Data: 03/03/2018\
-\}\
exer5 :: Integer -> Bool\
exer5 var \
  | var < -1 || (var > 1 && mod var 2 == 0) = True\
  | otherwise = False\
\
\
\{-\
-- Exercicio 6\
-- Fa\'e7a uma fun\'e7\'e3o que recebe um tipo Integer e retorna ele dividido por 2\
-- Autor: Pablo L. Leon\
-- Data: 03/03/2018\
-\}\
--(Num a) => Integer -> a\
exer6 :: Integer -> Double\
exer6 x = fromIntegral(x) \
\
\{-\
-- Exercicio 7\
-- Fa\'e7a uma fun\'e7\'e3o que receba um \'e2ngulo a e retorne uma tupla contendo o seno da metade desse \'e2ngulo utilizando a identidade:\
sin (x/2) = +- sqrt((1-cos(x))/2)\
-- Autor: Pablo L. Leon\
-- Data: 03/03/2018\
-\}\
\
exer7 :: Double -> (Double,Double)\
exer7 result = (cos1, cos2)\
  where\
    cos1 = -sqrt(cosenu)\
    cos2 = sqrt(cosenu)\
    cosenu = 1-(cos(result)/2)\
\{-\
-- Exercicio 8\
-- Crie uma lista de anos bissextos desde o ano 1 at\'e9 o atual.\
-- Autor: Pablo L. Leon\
-- Data: 06/03/2018\
-\}\
listabissexto = [x | x <- [1..2018], (x `mod` 400 == 0) || ((x `mod` 4 == 0) && (x `mod` 100 /= 0)) ]\
\
\{-\
-- Exercicio 9\
-- Encontre os 10 primeiros anos bissextos.\
-- Autor: Pablo L. Leon\
-- Data: 06/03/2018\
-\}\
listTop10bissextos :: [Integer]\
listTop10bissextos = take 10 $ listabissexto\
\
\{-\
-- Exercicio 9.a\
-- Encontre os 10 \'faltimos anos bissextos (dica: use a fun\'e7\'e3o length para determinar o tamanho da lista).\
-- Autor: Pablo L. Leon\
-- Data: 06/03/2018\
-\}\
listLast10bissextos :: [Integer]\
listLast10bissextos = drop ((length $ listabissexto) - 10) $ listabissexto\
\{-\
-- Exercicio 10\
-- Crie uma tupla em que o primeiro elemento tem metade dos anos bissextos e o segundo elemento a outra metade.\
-- Autor: Pablo L. Leon\
-- Data: 06/03/2018\
-\}\
exer10 :: [Integer] -> ([Integer], [Integer])\
exer10 listaAnos = splitAt (((length listaAnos) + 1) `div` 2) listaAnos\
\
\{-\
-- Exercicio 11\
-- Crie um concatenador de strings que concatena duas strings separadas por espa\'e7o.\
-- Autor: Pablo L. Leon\
-- Data: 07/03/2018\
-\}\
exer11 :: String -> String -> String\
exer11 str1 str2 = str1 ++ " " ++ str2\
\
\{-\
-- Exercicio 12\
-- Dada a string \'930123456789\'94, crie uma lista com os d\'edgitos em formato Integer.\
-\}\
exer12 :: String -> [Integer]\
exer12 listaNum = map (read .(:"")) listaNum :: [Integer]\
\
-- |'main' executa programa principal\
main :: IO ()\
main = do\
    print ("Exercicio 2")\
    print (mult3 5)\
    print ("Exercicio 3")\
    print (mult5 25)\
    print ("Exercicio 4")\
    print (mult35 19)\
    print ("Exercicio 5")\
    print (exer5 (-10))\
    print ("Exercio 6")\
    print (exer6 10)\
    print ("Exercicio 7")\
    print (exer7 10)\
    print ("Exercicio 8")\
    print (listabissexto)\
    print ("Exercicio 9")\
    print (listTop10bissextos)\
    print ("Exercicio 9.a")\
    print (listLast10bissextos)\
    print ("Exercicio 10")\
    print (exer10 listabissexto)\
    print ("Exercicio 11")\
    print (exer11 "Pablo" "UFABC S2")\
    print ("Exercicio 12")\
    print (exer12 "0123456789")}