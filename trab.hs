-- data Expr
--   = Booleano Bool
--   | Var Char Bool
--   | Nao Expr
--   | Conj Expr Expr
--   | Disj Expr Expr
--   deriving (Show, Eq)

-- evalExpr :: Expr -> Bool
-- evalExpr (Booleano x) = x
-- evalExpr (Var x b) = b
-- evalExpr (Nao x) = not (evalExpr x)
-- evalExpr (Conj x y) = evalExpr x && evalExpr y
-- evalExpr (Disj x y) = evalExpr x || evalExpr y

-- Tipos de token que a gente pode encontrar
data TokenExpr
  = Var Char
  | Nao
  | Conj
  | Disj
  | Implica
  | BiCond
  | AbParen
  | FeParen
  deriving (Show, Eq)

-- o lexer serve para ler a string e transformar em tokens que sao mais faceis de utilizar
lexer :: [Char] -> [TokenExpr]
lexer [] = []
lexer x
  | "(" `prefixOf` x = AbParen : lexer (drop 1 x)
  | ")" `prefixOf` x = FeParen : lexer (drop 1 x)
  | "v" `prefixOf` x = Disj : lexer (drop 1 x)
  | "ou" `prefixOf` x = Disj : lexer (drop 2 x)
  | "^" `prefixOf` x = Conj : lexer (drop 1 x)
  | "~" `prefixOf` x = Nao : lexer (drop 1 x)
  | "->" `prefixOf` x = Implica : lexer (drop 2 x)
  | "<->" `prefixOf` x = BiCond : lexer (drop 3 x)
  | head x `elem` ['A' .. 'Z'] = Var (head x) : lexer (drop 1 x)
  | " " `prefixOf` x = lexer (drop 1 x)
  | otherwise = error $ "caractere invalido: " ++ [head x]

-- verifica se uma string e o comeco da outra
-- 1o arg: prefixo para testar
-- 2o arg: string que vamos ver se tem o prefixo
prefixOf :: String -> String -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x : xs) (y : ys) = x == y && prefixOf xs ys

--                Tokens          Stack         Output        Resultado
shuntingYard :: [TokenExpr] -> [TokenExpr] -> [TokenExpr] -> [TokenExpr]
shuntingYard [] [] o = o
shuntingYard [] [s] o = shuntingYard [] [] (o ++ [s])
shuntingYard [] (AbParen : ss) o = error "parenteses abriu sem fechar"
shuntingYard [] (s : ss) o = shuntingYard [] ss (o ++ [s])
shuntingYard [Var r] s o = shuntingYard [] s (o ++ [Var r])
shuntingYard [FeParen] (AbParen : ss) o = shuntingYard [] ss o
shuntingYard [x] [] o = shuntingYard [] [x] o
shuntingYard [x] (s : ss) o
  | precedencia x > precedencia s = shuntingYard [] (x : s : ss) o
  | otherwise = shuntingYard [x] ss (o ++ [s])
shuntingYard (Var r : xs) s o = shuntingYard xs s (o ++ [Var r])
shuntingYard (AbParen : xs) s o = shuntingYard xs (AbParen : s) o
shuntingYard (FeParen : xs) (AbParen : ss) o = shuntingYard xs ss o
shuntingYard (FeParen : xs) (s : ss) o = shuntingYard (FeParen : xs) ss (o ++ [s])
shuntingYard (x : xs) [] o = shuntingYard xs [x] o
shuntingYard (x : xs) (s : ss) o
  | precedencia x > precedencia s = shuntingYard xs (x : s : ss) o
  | otherwise = shuntingYard (x : xs) ss (o ++ [s])

precedencia :: TokenExpr -> Int
precedencia Nao = 6
precedencia Conj = 5
precedencia Disj = 4
precedencia Implica = 3
precedencia BiCond = 2
precedencia AbParen = 1
precedencia FeParen = 1

main = do
  let str = "P -> (Q v R)"
  print str
  let l = lexer str
  print l
  let p = shuntingYard l [] []
  print p
