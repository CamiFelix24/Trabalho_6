{- Camila Przendziuk Franco Felix -}

{-Questão 1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado. -}
divisoresden :: Int -> [Int]
divisoresden x = [a | a <- [1..x], x `mod` a == 0]

{-Questão 2. Usando  List Comprehension  escreva  uma  função,  chamada  contaCaractere,  que  conte  a ocorrência de um caractere específico, em uma string dada. -}
contaCaractere :: String -> Char -> Int
contaCaractere x y = length [a | a <- x, a == y]

{-Questão 3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada. -}
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo x = [y*2 | y <- x, y >= 0]

{-Questão 4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado. -}
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras a = [(x, y, z) | x <-[1..a], y <-[1..a], z <- [1..a],
              (x ^ 2) + (y ^ 2) == (z ^ 2)]

{-Questão 5. Números  perfeitos  são  aqueles  cuja  soma  dos  seus  divisores  é  igual  ao  próprio  número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado. -}
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos x = [a | a <- [1..x], x `mod` a == 0]

{-Questão 6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis. -}
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar x y = sum [m * n | (m, n) <- zip x y]

{-Questão 7. Usando  List Comprehension  escreva  uma  função,  chamada  primeirosPrimos,  que  devolva uma lista contendo os n primeiros números primos a partir do número 2. -}
primeirosPrimos :: Int -> [Integer]
primeirosPrimos a = take a (b [2..])
  where b (x:xs) = x : b [c | c <- xs, rem c x > 0]

{-Questão 8. Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva uma  lista  de  par  ordenados  contendo  uma  potência  de  2  e  uma  potência  de  3  até  um determinado número dado. Observe que estes números podem ser bem grandes. -}
paresOrdenados :: Int -> [(Double, Double)]
paresOrdenados x = [(2 ^ y, 3 ^ y) | y <- [0 .. x]]

main = do
  putStrLn $ "\nFunc. 1: Entrada 20; Resultado: " ++ show (divisoresden 20)
  putStrLn $ "\nFunc. 2: Entrada Pneumoultramicroscopicossilicovulcanoconiótico 'o'; Resultado: " ++ show (contaCaractere "Pneumoultramicroscopicossilicovulcanoconiótico" 'o' )
  putStrLn $ "\nFunc. 3: Entrada [-2, 6, -8, 15, -3, 24]; Resultado: " ++ show (dobroNaoNegativo [-2, 6, -8, 15, -3, 24])
  putStrLn $ "\nFunc. 4: Entrada 10; Resultado: " ++ show (pitagoras 10)
  putStrLn $ "\nFunc. 5: Entrada 10; Resultado: " ++ show (numerosPerfeitos 10)
  putStrLn $ "\nFunc. 6: Entrada [6, 8, 4] [1, 5, 3]; Resultado: " ++ show (produtoEscalar [6, 8, 4] [1, 5, 3])
  putStrLn $ "\nFunc. 7: Entrada 10; Resultado: " ++ show (primeirosPrimos 10)
  putStrLn $ "\nFunc. 8: Entrada 10; Resultado: " ++ show (paresOrdenados 10)