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
  | "∨" `prefixOf` x = Disj : lexer (drop 1 x)
  | "ou" `prefixOf` x = Disj : lexer (drop 2 x)
  | "or" `prefixOf` x = Disj : lexer (drop 2 x)
  | "\\vee" `prefixOf` x = Disj : lexer (drop 4 x)
  -- e
  | "^" `prefixOf` x = Conj : lexer (drop 1 x)
  | "∧" `prefixOf` x = Conj : lexer (drop 1 x)
  | "e" `prefixOf` x = Conj : lexer (drop 1 x)
  | "and" `prefixOf` x = Conj : lexer (drop 3 x)
  | "\\wedge" `prefixOf` x = Conj : lexer (drop 6 x)
  -- nao
  | "~" `prefixOf` x = Nao : lexer (drop 1 x)
  | "¬" `prefixOf` x = Nao : lexer (drop 1 x)
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

------------------------- proposicao e transformacoes -------------------------

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
elimImpli (PBiCon x y) = elimImpli $ PConj (PImpli (elimImpli x) (elimImpli y)) (PImpli (elimImpli y) (elimImpli x))

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

-- chama a distributiva ate que nao haja mais mudancas
distributivaProp :: Prop -> Prop
distributivaProp p
  | p == distributivaPropDois p = p
  | otherwise = distributivaProp $ distributivaPropDois p

-- faz a distributiva parcial
distributivaPropDois :: Prop -> Prop
distributivaPropDois e = case e of
  PNao x -> PNao (distributivaPropDois x)
  PBool x -> PBool x
  PVar x -> PVar x
  PConj x y -> PConj (distributivaPropDois x) (distributivaPropDois y)
  PDisj x y -> case x of
    PConj x2 y2 -> distributivaPropDois (PConj (PDisj x2 y) (PDisj y2 y))
    _ -> case y of
      PConj x3 y3 -> distributivaPropDois (PConj (PDisj x x3) (PDisj x y3))
      _ -> PDisj (distributivaPropDois x) (distributivaPropDois y)

-- avalia a proposicao que nao tem mais variaveis, somente V ou F
avaliarProp :: Prop -> Bool
avaliarProp (PBool x) = x
avaliarProp (PNao x) = not $ avaliarProp x
avaliarProp (PConj x y) = avaliarProp x && avaliarProp y
avaliarProp (PDisj x y) = avaliarProp x || avaliarProp y
avaliarProp (PImpli x y) = not (avaliarProp x) || avaliarProp y
avaliarProp (PBiCon x y) = avaliarProp x == avaliarProp y

-- retorna uma lista de variaveis de uma proposicao
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
        | otherwise = Contingente (nub (zip variaveis (acharPrimeiro (zip resultados combinacoes) True))) (nub (zip variaveis (acharPrimeiro (zip resultados combinacoes) False)))
   in avaliaResultado resultados

-- cria combinacoes de V e F de uma lista de variaveis
criarCombinacoes :: [Char] -> [[Bool]]
criarCombinacoes c = replicateM (length c) [True, False]

-- remove duplicatas de uma lista
nub :: (Eq a) => [a] -> [a] -- remove todas as duplicatas de uma lista
nub [] = []
nub (x : xs)
  | x `elem` xs = nub xs
  | otherwise = x : nub xs

-- transforma a expressao para string
exprParaStr :: Prop -> String
exprParaStr (PVar x) = x : ""
exprParaStr (PNao x) = "~" ++ exprParaStr x
exprParaStr (PConj x y) = "(" ++ exprParaStr x ++ " ^ " ++ exprParaStr y ++ ")"
exprParaStr (PDisj x y) = "(" ++ exprParaStr x ++ " v " ++ exprParaStr y ++ ")"
exprParaStr (PImpli x y) = "(" ++ exprParaStr x ++ " -> " ++ exprParaStr y ++ ")"
exprParaStr (PBiCon x y) = "(" ++ exprParaStr x ++ " <-> " ++ exprParaStr y ++ ")"

-- transforma uma fnc para string
fncParaStr :: Prop -> String
fncParaStr (PVar r) = r : ""
fncParaStr (PNao x) = "~" ++ fncParaStr x
fncParaStr (PDisj x y) = fncParaStr x ++ " v " ++ fncParaStr y
fncParaStr (PConj x y) = "(" ++ fncParaStr x ++ ") ^ (" ++ fncParaStr y ++ ")"

------------------------- fnc e horn -------------------------

data Literal
  = VarPos Char
  | VarNeg Char
  deriving (Show, Eq)

type Clausula = [Literal]

eHorn :: Clausula -> Bool
eHorn [] = False -- talvez tenha que dar erro aqui
eHorn x = qtdPos <= 1
  where
    (qtdPos, qtdNeg) = contarLits x

contarLits :: Clausula -> (Int, Int)
contarLits [] = (0, 0)
contarLits [VarPos _] = (1, 0)
contarLits [VarNeg _] = (0, 1)
contarLits (VarPos _ : xs) = (1 + qtdPos, 0 + qtdNeg)
  where
    (qtdPos, qtdNeg) = contarLits xs
contarLits (VarNeg _ : xs) = (0 + qtdPos, 1 + qtdNeg)
  where
    (qtdPos, qtdNeg) = contarLits xs

disjParaClausula :: Prop -> Clausula
disjParaClausula (PVar r) = [VarPos r]
disjParaClausula (PNao (PVar r)) = [VarNeg r]
disjParaClausula (PDisj x y) = disjParaClausula x ++ disjParaClausula y
disjParaClausula (PConj _ _) = error "nunca deveria ter uma conjuncao aqui"

type FNC = [Clausula]

propFncParaFNC :: Prop -> FNC
propFncParaFNC (PConj x y) = propFncParaFNC x ++ propFncParaFNC y
propFncParaFNC (PDisj x y) = [disjParaClausula x ++ disjParaClausula y]
propFncParaFNC (PVar r) = [[VarPos r]]
propFncParaFNC (PNao (PVar r)) = [[VarNeg r]]

------------------------- avaliacao -------------------------

-- qual caso a expressao e
data Caso
  = Tautologia
  | Contradicao
  | Contingente [(Char, Bool)] [(Char, Bool)]
  deriving (Show, Eq)

-- ainda nao sei oq fazer
data ClausulaHorn = ClausulaHorn deriving (Show, Eq)

------------------------- funcao principal -------------------------

-- funcao que recebe o caso e retorna uma string em latex
toLatex :: Caso -> String
toLatex Tautologia = "$$\\text{Tautologia}$$"
toLatex Contradicao = "$$\\text{Contradição}$$"
toLatex (Contingente x y) = "$$\\text{Contingente}$$"

-- funcao principal
funcaoPrincipal :: String -> (Caso, ClausulaHorn, String, [TokenExpr], Prop, Prop)
funcaoPrincipal str = (caso, ClausulaHorn, toLatex caso, dpsShunt, prop, fnc)
  where
    lexado = lexer str
    dpsShunt = shuntingYard lexado [] []
    prop = transfTokenProp dpsShunt
    caso = avaliarCasoProp prop
    fnc = distributivaProp $ elimNeg $ elimImpli prop

------------------------- main -------------------------

main :: IO ()
main = do
  -- let str = "P ^ ~Q v R v ~V ^ A ^ B v ~C"
  let str = "(X v Y)"
  -- let str = "(A∨P∨R∨(¬C))∧(B∨P∨R∨(¬C))∧(A∨R∨(¬C)∨(¬Q))∧(B∨R∨(¬C)∨(¬Q))∧(P∨R∨(¬C)∨(¬S))∧(R∨(¬C)∨(¬Q)∨(¬S))"
  putStr $ "string: " ++ str
  putStr "\n"

  let (c, ch, s, t, e, f) = funcaoPrincipal str
  print c
  print ch
  print s
  print t
  print e
  putStrLn $ fncParaStr f
  putStrLn "depois"
  putStrLn "em fnc"
  print $ show $ propFncParaFNC f
  putStrLn "clausulas de horn"
  print $ show $ filter eHorn $ propFncParaFNC f

-- print $ avaliarCasoProp $ distributivaProp d
