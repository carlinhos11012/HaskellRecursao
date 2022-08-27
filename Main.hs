-- Aluno: Carlos Henrique Moreira dos Santos
import Text.Printf

{--
1. Escreva  uma  função  para  o  cálculo  dos  números  da  sequência  de  Fibonacci,  utilizando Haskell. 
--}
fibonacciAtual :: Int -> Int
fibonacciAtual 0 = 0
fibonacciAtual 1 = 1
fibonacciAtual x = fibonacciAtual (x-1) + fibonacciAtual (x-2)

fibonacci :: Int -> [Int]
fibonacci 0 = [0]
fibonacci x = fibonacci (x-1) ++ [fibonacciAtual x]

{--
2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum (MDC) de Euclides publicado por volta do ano 300 AC. Podemos simplificar este algoritmo dizendo que dados dois inteiros A e B, o MDC entre eles será dado pelo valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma função para o cálculo do MDC entre dois números inteiros positivos, usando o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.  
--}

euclides :: Int -> Int -> Int
euclides x y =
  if y == 0 then abs x
  else euclides y (x `mod` y)
  
{--
3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste número. Exemplo: dado 1234 a função deverá devolver 10. Utilizando Haskell e recursividade. 
--}
somaDigitos :: Int -> Int
somaDigitos 0 = 0
somaDigitos x = (x `mod` 10) + somaDigitos (x `div` 10)

{--
4. Escreva uma função que devolva a soma de todos os números menores que 10000 que sejam múltiplos de 3 ou 5. 
--}
mult3e5 :: Int -> Int
mult3e5 x =
  if x == 0 then 0
  else if x `mod` 3 == 0 || x `mod` 5 == 0 then x + mult3e5 (x-1)
  else mult3e5 (x-1)

{--
5. Escreva uma função que, recebendo uma lista de inteiros, apresente a diferença entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade. 
--}
somaQuadrado :: [Int] -> Int
somaQuadrado [] = 0
somaQuadrado (x:y) = x^2 + somaQuadrado y

diferencaQuadrado :: [Int] -> Int
diferencaQuadrado [] = 0
diferencaQuadrado x = somaQuadrado x - (sum x)^2

{--
6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado. 
--}
adicionarLista :: [Int] -> Int -> [Int]
adicionarLista x 0 = []
adicionarLista x 1 = []
adicionarLista x y = adicionarLista x (y-1) ++ [y]

removerElementos :: Int -> [Int] -> [Int]
removerElementos valor [] = []
removerElementos valor lista = 
  if (head lista) `mod` valor == 0 then removerElementos valor (tail lista) 
  else [head lista] ++ removerElementos valor (tail lista) 

metodoEuler :: [Int] -> [Int]
metodoEuler [] = []
metodoEuler lista = [head lista] ++ metodoEuler(removerElementos (head lista) (tail lista))

crivoEuler :: Int -> [Int]
crivoEuler x = metodoEuler (adicionarLista [] x)


{--
7. Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado. 
--}
lucasSequence :: Int -> [Int]
lucasSequence 0 = [2]
lucasSequence 1 = [2] ++ [1]
lucasSequence x = (lucasSequence (x-1)) ++ [((last(lucasSequence (x-1))) + (last(lucasSequence (x-2))))]

{--
8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1]. 
--}
aoContrario :: [Int] -> [Int]
aoContrario [] = []
aoContrario (x:y) = aoContrario y ++ [x]

{--
9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação. 
--}
somaRecursiva :: Int -> Int -> Int
somaRecursiva x 0 = 0
somaRecursiva x y = x + somaRecursiva x (y-1)

{--
10. Escreva uma função chamada comprimento que receba uma lista de inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista.
--}
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (x:y) = 1 + comprimento y

main = do
  --Ex1
  printf "\nFunc. 1: entrada:%d; resultado:%s\n" (7::Int) (show (fibonacci 7))

  --Ex2
  printf "\nFunc. 2: entrada:%d %d; resultado:%d\n" (1024::Int) (100::Int) (euclides 1024 100)

  -- Ex3
  printf "\nFunc. 3: entrada:%d; resultado:%d\n" (1024::Int) (somaDigitos 1024)

  --Ex4
  printf "\nFunc. 4: entrada:%d; resultado:%d\n" (6::Int) (mult3e5 6)

  --Ex5
  printf "\nFunc. 5: entrada:%s; resultado:%d\n" (show ([1,2,3]::[Int])) (diferencaQuadrado [1,2,3])

  --Ex6
  printf "\nFunc. 6: entrada:%d; resultado:%s\n" (50::Int) (show (crivoEuler 50))

  --Ex7
  printf "\nFunc. 7: entrada:%d; resultado:%s\n" (11::Int) (show (lucasSequence 11))

  --Ex8
  printf "\nFunc. 8: entrada:%s; resultado:%s\n" (show ([2,3,4,5]::[Int])) (show (aoContrario [2,3,4,5]))

  --Ex9
  printf "\nFunc. 9: entrada:%d %d; resultado:%d\n" (8::Int) (6::Int) (somaRecursiva 8 6)

  --Ex10
  printf "\nFunc. 10: entrada:%s; resultado:%d\n" (show ([3,4,2,5]::[Int])) (comprimento [3,4,2,5])