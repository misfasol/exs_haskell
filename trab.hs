-- Tipos de token que a gente pode encontrar
data TokenExpr
  = Var Char
  | Booleano Bool
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
shuntingYard [] [] o = o -- caso base
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

-- verifica a precedencia do token
precedencia :: TokenExpr -> Int
precedencia Nao = 6
precedencia Conj = 5
precedencia Disj = 4
precedencia Implica = 3
precedencia BiCond = 2
precedencia AbParen = 1
precedencia FeParen = 1

avaliarCaso :: [TokenExpr] -> Caso
avaliarCaso _ = Tautologia

-- avalia uma lista de tokens que nao tem mais variaveis, somente V ou F e operadores
avaliarExpr :: [TokenExpr] -> [TokenExpr] -> Bool
avaliarExpr [] [] = error "input vazio"
avaliarExpr [] [Booleano b] = b
avaliarExpr x (Nao : Booleano b : ss) = avaliarExpr x (Booleano (not b) : ss)
avaliarExpr x (Conj : Booleano a : Booleano b : ss) = avaliarExpr x (Booleano (a && b) : ss)
avaliarExpr x (Disj : Booleano a : Booleano b : ss) = avaliarExpr x (Booleano (a || b) : ss)
avaliarExpr x (Implica : Booleano a : Booleano b : ss) = avaliarExpr x (Booleano (not a || b) : ss)
avaliarExpr x (BiCond : Booleano a : Booleano b : ss) = avaliarExpr x (Booleano (a == b) : ss)
avaliarExpr (x : xs) s = avaliarExpr xs (x : s)

-- troca todas as variaveis por V
trocarPorV :: [TokenExpr] -> [TokenExpr]
trocarPorV [] = []
trocarPorV [Var r] = [Booleano True]
trocarPorV [x] = [x]
trocarPorV (Var r : xs) = Booleano True : trocarPorV xs
trocarPorV (x : xs) = x : trocarPorV xs

-- retorna lista somente com variaveis
variaveisExpr :: [TokenExpr] -> [TokenExpr]
variaveisExpr = filter eVariavel

-- retorna se e variavel
eVariavel :: TokenExpr -> Bool
eVariavel (Var _) = True
eVariavel _ = False

-- qual caso a expressao e
data Caso
  = Tautologia
  | Contradicao
  | Contingente [TokenExpr] [TokenExpr]

-- ainda nao sei oq fazer
data ClausulaHorn = ClausulaHorn

-- funcao principal
funcaoPrincipal :: String -> (Caso, ClausulaHorn)
funcaoPrincipal str = (caso, ClausulaHorn)
  where
    lexado = lexer str
    dpsShunt = shuntingYard lexado [] []
    caso = avaliarCaso dpsShunt

main = do
  let str = "P v Q ^ R"
  print str
  let l = lexer str
  print l
  let p = shuntingYard l [] []
  print p
  let a = [Var 'R', Var 'P', Conj]
  print a
  let trocado = trocarPorV a
  print trocado
  print $ avaliarExpr [Booleano False, Booleano False, Booleano True, Conj, Disj] []
