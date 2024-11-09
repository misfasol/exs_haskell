pres :: (Eq a) => a -> [a] -> Bool -- diz se um valor esta presente na lista
pres _ [] = False
pres a (x : xs)
  | a == x = True
  | otherwise = pres a xs

nub :: (Eq a) => [a] -> [a] -- remove todas as duplicatas de uma lista
nub [] = []
nub (x : xs)
  | pres x xs = nub xs
  | otherwise = x : nub xs

menorLista :: [Int] -> Int -- retorna o menor valor da lista
menorLista [] = undefined
menorLista [x] = x
menorLista (x : xs)
  | x < head xs = menorLista (x : tail xs)
  | otherwise = menorLista xs

isAsc :: [Int] -> Bool -- diz se uma lista e ascendente
isAsc [] = True
isAsc [_] = True
isAsc (x : xs)
  | x <= menorLista xs = isAsc xs
  | otherwise = False

outraIsAsc :: [Int] -> Bool -- outra foram do asc feita pelo cara do video
outraIsAsc [] = True
outraIsAsc [x] = True
outraIsAsc (x : y : xs) = (x <= y) && outraIsAsc (y : xs)

hasPath :: [(Int, Int)] -> Int -> Int -> Bool -- feito pelo cara do video
hasPath [] x y = x == y
hasPath xs x y
  | x == y = True
  | otherwise =
      let xs' = [(n, m) | (n, m) <- xs, n /= x]
       in or [hasPath xs' m y | (n, m) <- xs, n == x]

revList :: [Int] -> [Int] -- msm coisa q reverse
revList = foldl (flip (:)) []

main = do
  print $ pres 4 [1, 2, 3]
  print $ nub [1, 2, 3, 2]
