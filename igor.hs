data Proposicao
  = Var Char
  | Nao Proposicao
  | E Proposicao Proposicao
  | Ou Proposicao Proposicao
  | Implica Proposicao Proposicao
  | Bicondicional Proposicao Proposicao
  deriving (Show, Eq)

interpretarProposicao :: String -> Proposicao
interpretarProposicao expr = fst (parseExpr (filter (/= ' ') expr))

-- Função de parser para analisar a expressão
parseExpr :: String -> (Proposicao, String)
parseExpr ('¬' : resto) =
  let (subExpr, resto') = parseExpr resto
   in (Nao subExpr, resto')
parseExpr ('~' : resto) =
  let (subExpr, resto') = parseExpr resto
   in (Nao subExpr, resto')
parseExpr ('n' : 'o' : 't' : resto) =
  let (subExpr, resto') = parseExpr resto
   in (Nao subExpr, resto')
parseExpr (x : xs)
  | x `elem` ['A' .. 'Z'] = (Var x, xs)
parseExpr ('(' : resto) =
  let (esq, resto') = parseExpr resto
      (operador, resto'') = takeOperador resto'
      (dir, restoFinal) = parseExpr resto''
   in case operador of
        "∧" -> (E esq dir, tail restoFinal)
        "^" -> (E esq dir, tail restoFinal)
        "and" -> (E esq dir, tail restoFinal)
        "∨" -> (Ou esq dir, tail restoFinal)
        "v" -> (Ou esq dir, tail restoFinal)
        "or" -> (Ou esq dir, tail restoFinal)
        "=>" -> (Implica esq dir, tail restoFinal)
        "->" -> (Implica esq dir, tail restoFinal)
        "<=>" -> (Bicondicional esq dir, tail restoFinal)
        "<->" -> (Bicondicional esq dir, tail restoFinal)
        _ -> error $ "Operador inválido ou ausente: " ++ operador
parseExpr _ = error "Expressão inválida"

takeOperador :: String -> (String, String)
takeOperador expr
  | null expr = error "Operador esperado, mas não encontrado"
  | "∧" `prefixOf` expr = ("∧", drop 1 expr)
  | "^" `prefixOf` expr = ("^", drop 1 expr)
  | "and" `prefixOf` expr = ("and", drop 3 expr)
  | "∨" `prefixOf` expr = ("∨", drop 1 expr)
  | "v" `prefixOf` expr = ("v", drop 1 expr)
  | "or" `prefixOf` expr = ("or", drop 2 expr)
  | "→" `prefixOf` expr = ("→", drop 1 expr)
  | "=>" `prefixOf` expr = ("=>", drop 2 expr)
  | "->" `prefixOf` expr = ("->", drop 2 expr)
  | "↔" `prefixOf` expr = ("↔", drop 1 expr)
  | "<=>" `prefixOf` expr = ("<=>", drop 3 expr)
  | "<->" `prefixOf` expr = ("<->", drop 3 expr)
  | otherwise = error $ "Operador inválido ou ausente: " ++ expr

-- dropOperador :: String -> String -> String
-- dropOperador op expr = drop (length op) expr

prefixOf :: String -> String -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (p : ps) (x : xs)
  | p == x = prefixOf ps xs
  | otherwise = False

contarVariaveis :: Proposicao -> Int
contarVariaveis (Var x) = 1
contarVariaveis (Nao x) = contarVariaveis x
contarVariaveis (E x y) = contarVariaveis x + contarVariaveis y
contarVariaveis (Ou x y) = contarVariaveis x + contarVariaveis y
contarVariaveis (Implica x y) = contarVariaveis x + contarVariaveis y
contarVariaveis (Bicondicional x y) = contarVariaveis x + contarVariaveis y

main :: IO ()
main = do
  let expr = "()"
  let filtrado = filter (/= ' ') (('(' : expr) ++ ")")
  print $ filtrado
  let prop = interpretarProposicao filtrado
  let qtd = contarVariaveis prop
  print prop
  print qtd
