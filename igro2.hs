import Control.Monad

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

------------------------- lexer -------------------------

-- o lexer serve para ler a string e transformar em tokens que sao mais faceis de utilizar
lexer :: [Char] -> [TokenExpr]
lexer [] = []
lexer x
  -- abre parenteses
  | "(" `prefixOf` x = AbParen : lexer (drop 1 x)
  | ('\\' : "(") `prefixOf` x = AbParen : lexer (drop 2 x)
  -- fecha parenteses
  | ")" `prefixOf` x = FeParen : lexer (drop 1 x)
  | ('\\' : ")") `prefixOf` x = FeParen : lexer (drop 2 x)
  -- ou
  | "v" `prefixOf` x = Disj : lexer (drop 1 x)
  | "ou" `prefixOf` x = Disj : lexer (drop 2 x)
  | "or" `prefixOf` x = Disj : lexer (drop 2 x)
  | ('\\' : "vee") `prefixOf` x = Disj : lexer (drop 4 x)
  -- e
  | "^" `prefixOf` x = Conj : lexer (drop 1 x)
  | "and" `prefixOf` x = Conj : lexer (drop 3 x)
  | "e" `prefixOf` x = Conj : lexer (drop 1 x)
  | ('\\' : "wedge") `prefixOf` x = Conj : lexer (drop 6 x)
  -- nao
  | "~" `prefixOf` x = Nao : lexer (drop 1 x)
  | "not" `prefixOf` x = Nao : lexer (drop 3 x)
  | ('\\' : "neg") `prefixOf` x = Nao : lexer (drop 4 x)
  -- implicacao
  | "->" `prefixOf` x = Implica : lexer (drop 2 x)
  | "=>" `prefixOf` x = Implica : lexer (drop 2 x)
  | "→" `prefixOf` x = Implica : lexer (drop 1 x)
  | ('\\' : "Rightarrow") `prefixOf` x = Implica : lexer (drop 11 x)
  | ('\\' : "implies") `prefixOf` x = Implica : lexer (drop 8 x)
  -- bicondicional
  | "<->" `prefixOf` x = BiCond : lexer (drop 3 x)
  | "<=>" `prefixOf` x = BiCond : lexer (drop 3 x)
  | "↔" `prefixOf` x = BiCond : lexer (drop 1 x)
  | ('\\' : "Leftrightarrow") `prefixOf` x = BiCond : lexer (drop 15 x)
  -- variavel
  | head x `elem` ['A' .. 'Z'] = Var (head x) : lexer (drop 1 x)
  -- outros
  | " " `prefixOf` x = lexer (drop 1 x)
  | "\t" `prefixOf` x = lexer (drop 1 x)
  | otherwise = error $ "caractere invalido: " ++ [head x]

-- verifica se uma string e o comeco da outra
-- 1o arg: prefixo para testar
-- 2o arg: string que vamos ver se tem o prefixo
prefixOf :: String -> String -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x : xs) (y : ys) = x == y && prefixOf xs ys

------------------------- parser -------------------------

-- algoritmo de shuntingYard, ele transforma uma expressao em infix para postfix,
-- que e mais facil de computar o resultado
--
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

------------------------- avaliacao -------------------------

-- qual caso a expressao e
data Caso
  = Tautologia
  | Contradicao
  | Contingente
  deriving (Show, Eq)

-- ainda nao sei oq fazer
data ClausulaHorn = ClausulaHorn deriving (Show, Eq)

-- avalia o caso da expressao que ja esta shunted fornecida
avaliarCaso :: [TokenExpr] -> Caso
avaliarCaso x =
  let variaveis = variaveisExpr x
      combinacao = criarCombinacoes variaveis
      resultados = [avaliarExpr (trocarVariaveis x l) [] | l <- combinacao]
      -- retorna o que a expressao e baseado na lista de resultados
      avaliaResultado :: [Bool] -> Caso
      avaliaResultado lb
        | and lb = Tautologia
        | all not lb = Contradicao
        | otherwise = Contingente
   in avaliaResultado resultados

-- retorna lista de tokens somente com as letras das variaveis
variaveisExpr :: [TokenExpr] -> [Char]
variaveisExpr x = map charDaVar (filter eVariavel x)
  where
    -- retorna o caractece de uma Variavel
    charDaVar :: TokenExpr -> Char
    charDaVar (Var r) = r
    -- retorna se e uma Variavel ou nao
    eVariavel :: TokenExpr -> Bool
    eVariavel (Var _) = True
    eVariavel _ = False

-- cria combinacoes de V e F de uma lista de variaveis
criarCombinacoes :: [Char] -> [[Bool]]
criarCombinacoes c = replicateM (length c) [True, False]

-- troca as variaveis de uma expressao por V ou F seguindo a lista
trocarVariaveis :: [TokenExpr] -> [Bool] -> [TokenExpr]
trocarVariaveis lt lb =
  let tabela = zip (variaveisExpr lt) lb
      -- converte uma Variavel para um Booleano dependendo do que ele e na tabela criada
      conv :: TokenExpr -> TokenExpr
      conv (Var r)
        | (r, True) `elem` tabela = Booleano True
        | (r, False) `elem` tabela = Booleano False
      conv to = to
   in map conv lt

-- avalia uma lista de tokens que nao tem mais variaveis, somente V ou F e operadores
avaliarExpr :: [TokenExpr] -> [TokenExpr] -> Bool
avaliarExpr [] [] = error "input vazio"
avaliarExpr [] [Booleano b] = b -- caso base
avaliarExpr x (Nao : Booleano b : ss) = avaliarExpr x (Booleano (not b) : ss)
avaliarExpr x (Conj : Booleano a : Booleano b : ss) = avaliarExpr x (Booleano (a && b) : ss)
avaliarExpr x (Disj : Booleano a : Booleano b : ss) = avaliarExpr x (Booleano (a || b) : ss)
avaliarExpr x (Implica : Booleano a : Booleano b : ss) = avaliarExpr x (Booleano (not a || b) : ss)
avaliarExpr x (BiCond : Booleano a : Booleano b : ss) = avaliarExpr x (Booleano (a == b) : ss)
avaliarExpr (x : xs) s = avaliarExpr xs (x : s) -- jogar o primeiro item do input na stack

------------------------- funcao principal -------------------------

--
toLatex :: Caso -> String
toLatex Tautologia = "$$\\text{Tautologia}$$"
toLatex Contradicao = "$$\\text{Contradição}$$"
toLatex Contingente = "$$\\text{Contingente}$$"

--
funcaoPrincipalLatex :: String -> IO ()
funcaoPrincipalLatex str = do
  let lexado = lexer str
  let dpsShunt = shuntingYard lexado [] []
  let caso = avaliarCaso dpsShunt
  putStrLn $ "Expressão original (LaTeX): $$" ++ str ++ "$$"
  putStrLn $ "Resultado (terminal): " ++ show caso
  putStrLn $ "Resultado (LaTeX): " ++ toLatex caso

------------------------- main -------------------------

main :: IO ()
main = do
  let str = "P \\vee (Q \\wedge \\neg Q) \\Leftrightarrow P"
  putStrLn "Expressão original:"
  putStrLn str
  funcaoPrincipalLatex str
