--Questão 1: Defina uma função recursiva que receba uma lista de números inteiros e produza uma nova lista com cada número elevado ao quadrado.

quad :: [Int] -> [Int]
quad [] = []
quad n = head(n)^2 : quad(tail n) 

--Questão 2: Escreva uma função recursiva que receba uma lista de nomes e adicione a string "Sr. " no início de cada nome.

addSr :: [String] -> [String]
addSr []=[]
addSr list = ("Sr. " ++(head list)) : addSr(tail list)

--Questão 3: Crie uma função recursiva que receba uma string e retorne o número de espaços nela contidos.

null_ :: String -> Int 
null_ [] = 0
null_ list = if ((head list) == ' ')
then 1 + null_(tail list)
else 0 + null_(tail list) 

--Questão 4:Escreva uma função recursiva que, dada uma lista de números, calcule 3*n^2 + 2/n + 1 para cada número n da lista.

result :: [Float] -> [Float]
result [] = []
result n = (3*(head n)^2 + 2/(head n) + 1) : result(tail n) 

--Questão 5: Escreva uma função recursiva que, dada uma lista de números, selecione somente os que forem negativos.
negat :: [Float] -> [Float]
negat [] = []
negat list = if ((head list) < 0)
  then  head(list) : negat(tail list)
  else  negat (tail list)

--Questão 6:Defina uma função não-recursiva que receba uma string e retire suas vogais, conforme os exemplos abaixo.

semVogais_  :: String -> String 
semVogais_  lista = filter(\n ->not(n`elem` "aeiouAEIOU"))lista


--Questão 7: Recursiva da 6. 

semVogais :: String -> String
semVogais "" = ""
semVogais lista = if(elem (head lista) "aeiouAEIOU") 
then semVogais (tail lista) else head lista : semVogais (tail lista)

--Questão 8: 
semLetras :: String -> String
semLetras lista = map(\n -> if n == ' ' then ' ' else '-')lista

--Questão 9: Recursiva da 8

semLetras_ :: String -> String
semLetras_ ""=""
semLetras_ lista = if((head lista) == ' ')
then head(lista) : semLetras_(tail lista) 
else '-':semLetras_(tail lista)

--Questão 10: Crie uma função recursiva charFound :: Char -> String -> Bool, que verifique se o caracter (primeiro argumento) está contido na string (segundo argumento).
charFound :: Char -> String -> Bool
charFound _ [] = False
charFound x (y:ys) | x == y    = True
                | otherwise = charFound x ys


--Questão 11: Defina uma função recursiva que receba uma lista de coordenadas de pontos 2D e desloque esses pontos em 2 unidades, conforme o exemplo abaixo
translate :: [(Float, Float)] -> [(Float, Float)]
translate [] = []
translate ((x,y):rest) = (x + 2, y + 2) : translate rest

--Questão 12:Defina uma função recursiva que receba 2 listas e retorne uma lista contendo o produto, par a par, dos elementos das listas de entrada. 

prod :: [Int] -> [Int] -> [Int]
prod [] _ = []
prod _ [] = []
prod (x:xs) (y:ys) = x*y : prod xs ys

--Questão 13: Resolva o exercício anterior usando uma função de alta ordem, eliminando a necessidade de escrever código com recursão.
prod_ :: [Int] -> [Int] -> [Int]
prod_ l1 l2 = zipWith (*) l1 l2


--Questão 14: Defina uma função recursiva que receba um número n e retorne uma tabela de números de 1 a n e seus quadrados
geraTabela :: Int -> [(Int,Int)]
geraTabela 0 = [(0, 0)]
geraTabela n = aux 1 n

aux :: Int -> Int -> [(Int, Int)]
aux h n = if(h  <= n)
   then (h, h^2) : aux(h + 1) n
   else []






