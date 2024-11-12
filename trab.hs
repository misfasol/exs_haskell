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
  | "\\(" `prefixOf` x = AbParen : lexer (drop 2 x)
  -- fecha parenteses
  | ")" `prefixOf` x = FeParen : lexer (drop 1 x)
  | "\\)" `prefixOf` x = FeParen : lexer (drop 2 x)
  -- ou
  | "v" `prefixOf` x = Disj : lexer (drop 1 x)
  | "ou" `prefixOf` x = Disj : lexer (drop 2 x)
  | "or" `prefixOf` x = Disj : lexer (drop 2 x)
  | "\\vee" `prefixOf` x = Disj : lexer (drop 4 x)
  -- e
  | "^" `prefixOf` x = Conj : lexer (drop 1 x)
  | "e" `prefixOf` x = Conj : lexer (drop 1 x)
  | "and" `prefixOf` x = Conj : lexer (drop 3 x)
  | "\\wedge" `prefixOf` x = Conj : lexer (drop 6 x)
  -- nao
  | "~" `prefixOf` x = Nao : lexer (drop 1 x)
  | "not" `prefixOf` x = Nao : lexer (drop 3 x)
  | "\\neg" `prefixOf` x = Nao : lexer (drop 4 x)
  -- implicacao
  | "->" `prefixOf` x = Implica : lexer (drop 2 x)
  | "=>" `prefixOf` x = Implica : lexer (drop 2 x)
  | "→" `prefixOf` x = Implica : lexer (drop 1 x)
  | "\\Rightarrow" `prefixOf` x = Implica : lexer (drop 11 x)
  | "\\implies" `prefixOf` x = Implica : lexer (drop 8 x)
  -- bicondicional
  | "<->" `prefixOf` x = BiCond : lexer (drop 3 x)
  | "<=>" `prefixOf` x = BiCond : lexer (drop 3 x)
  | "↔" `prefixOf` x = BiCond : lexer (drop 1 x)
  | "\\Leftrightaarow" `prefixOf` x = BiCond : lexer (drop 15 x)
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
  | precedencia x >= precedencia s = shuntingYard [] (x : s : ss) o
  | otherwise = shuntingYard [x] ss (o ++ [s])
shuntingYard (Var r : xs) s o = shuntingYard xs s (o ++ [Var r])
shuntingYard (AbParen : xs) s o = shuntingYard xs (AbParen : s) o
shuntingYard (FeParen : xs) (AbParen : ss) o = shuntingYard xs ss o
shuntingYard (FeParen : xs) (s : ss) o = shuntingYard (FeParen : xs) ss (o ++ [s])
shuntingYard (x : xs) [] o = shuntingYard xs [x] o
shuntingYard (x : xs) (s : ss) o
  | precedencia x >= precedencia s = shuntingYard xs (x : s : ss) o
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

------------------------- expressao e transformacoes -------------------------

-- proposicao
data Prop
  = PVar Char
  | PBool Bool
  | PNao Prop
  | PConj Prop Prop
  | PDisj Prop Prop
  | PImpli Prop Prop
  | PBiCon Prop Prop
  deriving (Show, Eq)

-- transformar lista de tokens para proposicao
-- como ta em notacao polonesa reversa, tem que inverter antes de jogar na funcao
transfTokenProp :: [TokenExpr] -> Prop
transfTokenProp x = fst $ transfTokenPropDois $ reverse x

-- a funcao de transformacao em si
-- inverte x e y por antes estar em notacao polonesa reversa
transfTokenPropDois :: [TokenExpr] -> (Prop, [TokenExpr])
transfTokenPropDois (Var r : xs) = (PVar r, xs)
transfTokenPropDois (Nao : xs) = (PNao x, xs')
  where
    (x, xs') = transfTokenPropDois xs
transfTokenPropDois (Conj : xs) = (PConj y x, xs'')
  where
    (x, xs') = transfTokenPropDois xs
    (y, xs'') = transfTokenPropDois xs'
transfTokenPropDois (Disj : xs) = (PDisj y x, xs'')
  where
    (x, xs') = transfTokenPropDois xs
    (y, xs'') = transfTokenPropDois xs'
transfTokenPropDois (Implica : xs) = (PImpli y x, xs'')
  where
    (x, xs') = transfTokenPropDois xs
    (y, xs'') = transfTokenPropDois xs'
transfTokenPropDois (BiCond : xs) = (PBiCon y x, xs'')
  where
    (x, xs') = transfTokenPropDois xs
    (y, xs'') = transfTokenPropDois xs'

-- transformar as variaveis em PBool de uma proposicao
trocarVarProp :: Prop -> [Bool] -> Prop
trocarVarProp p lb =
  let tabela = zip (variaveisProp p) lb
      conv :: Prop -> Prop
      conv (PVar r)
        | (r, True) `elem` tabela = PBool True
        | (r, False) `elem` tabela = PBool False
      conv (PBool b) = PBool b
      conv (PNao x) = PNao (conv x)
      conv (PConj x y) = PConj (conv x) (conv y)
      conv (PDisj x y) = PDisj (conv x) (conv y)
      conv (PImpli x y) = PImpli (conv x) (conv y)
      conv (PBiCon x y) = PBiCon (conv x) (conv y)
   in conv p

-- eliminar implicacoes
elimImpli :: Prop -> Prop
elimImpli (PVar x) = PVar x
elimImpli (PNao x) = PNao (elimImpli x)
elimImpli (PConj x y) = PConj (elimImpli x) (elimImpli y)
elimImpli (PDisj x y) = PDisj (elimImpli x) (elimImpli y)
elimImpli (PImpli x y) = PDisj (PNao (elimImpli x)) (elimImpli y)
elimImpli (PBiCon x y) = PConj (PImpli (elimImpli x) (elimImpli y)) (PImpli (elimImpli y) (elimImpli x))

-- eliminar negacoes
-- nao e pra ter mais implicacao nem bicondicional
elimNeg :: Prop -> Prop
elimNeg (PVar x) = PVar x
elimNeg (PConj x y) = PConj (elimNeg x) (elimNeg y)
elimNeg (PDisj x y) = PDisj (elimNeg x) (elimNeg y)
elimNeg (PNao x) = case x of
  PVar a -> PNao (PVar a)
  PNao a -> elimNeg a
  PConj a b -> elimNeg (PDisj (PNao (elimNeg a)) (PNao (elimNeg b)))
  PDisj a b -> elimNeg (PConj (PNao (elimNeg a)) (PNao (elimNeg b)))

-- avalia a proposicao que nao tem mais variaveis, somente V ou F
avaliarProp :: Prop -> Bool
avaliarProp (PBool x) = x
avaliarProp (PNao x) = not $ avaliarProp x
avaliarProp (PConj x y) = avaliarProp x && avaliarProp y
avaliarProp (PDisj x y) = avaliarProp x || avaliarProp y
avaliarProp (PImpli x y) = not (avaliarProp x) || avaliarProp y
avaliarProp (PBiCon x y) = avaliarProp x == avaliarProp y

variaveisProp :: Prop -> [Char]
variaveisProp (PBool b) = []
variaveisProp (PVar r) = [r]
variaveisProp (PNao x) = variaveisProp x
variaveisProp (PConj x y) = variaveisProp x ++ variaveisProp y
variaveisProp (PDisj x y) = variaveisProp x ++ variaveisProp y
variaveisProp (PImpli x y) = variaveisProp x ++ variaveisProp y
variaveisProp (PBiCon x y) = variaveisProp x ++ variaveisProp y

-- avalia o caso de uma proposicao
avaliarCasoProp :: Prop -> Caso
avaliarCasoProp x =
  let variaveis = variaveisProp x
      combinacoes = criarCombinacoes variaveis
      resultados = [avaliarProp (trocarVarProp x l) | l <- combinacoes]
      -- funcao que acha a primeira ocorrencia de True ou False nos resultados
      acharPrimeiro :: [(Bool, [Bool])] -> Bool -> [Bool]
      acharPrimeiro [] x = error "utilizacao da funcao acharPrimeiro em lista vazia (talvez nao seja contingencia)" -- como so vamos chamar em contigencia nunca deveria chegar aqui
      acharPrimeiro [(x, l)] b
        | b == x = l
        | otherwise = error "utilizacao da funcao acharPrimeiro em nao contingencia"
      acharPrimeiro ((x, l) : xs) b
        | x == b = l
        | otherwise = acharPrimeiro xs b
      -- funcao que avalia os resultados
      avaliaResultado :: [Bool] -> Caso
      avaliaResultado lb
        | and lb = Tautologia
        | all not lb = Contradicao
        | otherwise = Contingente (zip variaveis (acharPrimeiro (zip resultados combinacoes) True)) (zip variaveis (acharPrimeiro (zip resultados combinacoes) False))
   in avaliaResultado resultados

-- transforma a expressao para string
exprParaStr :: Prop -> String
exprParaStr (PVar x) = x : ""
exprParaStr (PNao x) = "~" ++ exprParaStr x
exprParaStr (PConj x y) = "(" ++ exprParaStr x ++ " ^ " ++ exprParaStr y ++ ")"
exprParaStr (PDisj x y) = "(" ++ exprParaStr x ++ " v " ++ exprParaStr y ++ ")"
exprParaStr (PImpli x y) = "(" ++ exprParaStr x ++ " -> " ++ exprParaStr y ++ ")"
exprParaStr (PBiCon x y) = "(" ++ exprParaStr x ++ " <-> " ++ exprParaStr y ++ ")"

------------------------- avaliacao -------------------------

-- qual caso a expressao e
data Caso
  = Tautologia
  | Contradicao
  | Contingente [(Char, Bool)] [(Char, Bool)]
  deriving (Show, Eq)

-- ainda nao sei oq fazer
data ClausulaHorn = ClausulaHorn deriving (Show, Eq)

-- avalia o caso da expressao que ja esta shunted fornecida
-- avaliarCaso :: [TokenExpr] -> Caso
-- avaliarCaso x =
--   let variaveis = variaveisExpr x
--       combinacao = criarCombinacoes variaveis
--       resultados = [avaliarExpr (trocarVariaveis x l) [] | l <- combinacao]
--       -- retorna o que a expressao e baseado na lista de resultados
--       avaliaResultado :: [Bool] -> Caso
--       avaliaResultado lb
--         | and lb = Tautologia
--         | all not lb = Contradicao
--         | otherwise = Contingente
--    in avaliaResultado resultados

-- retorna lista de tokens somente com as letras das variaveis
-- variaveisExpr :: [TokenExpr] -> [Char]
-- variaveisExpr x = map charDaVar (filter eVariavel x)
--   where
--     -- retorna o caractece de uma Variavel
--     charDaVar :: TokenExpr -> Char
--     charDaVar (Var r) = r
--     -- retorna se e uma Variavel ou nao
--     eVariavel :: TokenExpr -> Bool
--     eVariavel (Var _) = True
--     eVariavel _ = False

-- cria combinacoes de V e F de uma lista de variaveis
criarCombinacoes :: [Char] -> [[Bool]]
criarCombinacoes c = replicateM (length c) [True, False]

-- troca as variaveis de uma expressao por V ou F seguindo a lista
-- trocarVariaveis :: [TokenExpr] -> [Bool] -> [TokenExpr]
-- trocarVariaveis lt lb =
--   let tabela = zip (variaveisExpr lt) lb
--       -- converte uma Variavel para um Booleano dependendo do que ele e na tabela criada
--       conv :: TokenExpr -> TokenExpr
--       conv (Var r)
--         | (r, True) `elem` tabela = Booleano True
--         | (r, False) `elem` tabela = Booleano False
--       conv to = to
--    in map conv lt

-- avalia uma lista de tokens que nao tem mais variaveis, somente V ou F e operadores
-- avaliarExpr :: [TokenExpr] -> [TokenExpr] -> Bool
-- avaliarExpr [] [] = error "input vazio"
-- avaliarExpr [] [Booleano b] = b -- caso base
-- avaliarExpr x (Nao : Booleano b : ss) = avaliarExpr x (Booleano (not b) : ss)
-- avaliarExpr x (Nao : Nao : Booleano b : ss) = avaliarExpr x (Booleano b : ss)
-- avaliarExpr x (Conj : Booleano a : Booleano b : ss) = avaliarExpr x (Booleano (a && b) : ss)
-- avaliarExpr x (Disj : Booleano a : Booleano b : ss) = avaliarExpr x (Booleano (a || b) : ss)
-- avaliarExpr x (Implica : Booleano a : Booleano b : ss) = avaliarExpr x (Booleano (not a || b) : ss)
-- avaliarExpr x (BiCond : Booleano a : Booleano b : ss) = avaliarExpr x (Booleano (a == b) : ss)
-- avaliarExpr (x : xs) s = avaliarExpr xs (x : s) -- jogar o primeiro item do input na stack

------------------------- funcao principal -------------------------

-- funcao que recebe o caso e retorna uma string em latex
toLatex :: Caso -> String
toLatex Tautologia = "$$\\text{Tautologia}$$"
toLatex Contradicao = "$$\\text{Contradição}$$"
toLatex (Contingente x y) = "$$\\text{Contingente}$$"

-- funcao principal
funcaoPrincipal :: String -> (Caso, ClausulaHorn, String, [TokenExpr], Prop)
funcaoPrincipal str = (caso, ClausulaHorn, toLatex caso, dpsShunt, prop)
  where
    lexado = lexer str
    dpsShunt = shuntingYard lexado [] []
    prop = transfTokenProp dpsShunt
    caso = avaliarCasoProp prop

------------------------- main -------------------------

main :: IO ()
main = do
  let str = "A -> B"
  putStr $ "string: " ++ str
  putStr "\n"

  let (c, ch, s, t, e) = funcaoPrincipal str
  print c
  print ch
  print s
  print t
  print e
  putStrLn $ exprParaStr $ elimNeg $ elimImpli e
