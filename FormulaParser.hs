module FormulaParser where
import FormulaTokenizer as T
import Formula as F

parseTerm :: [Token] -> Maybe (Term, [Token])
parseTerm ((Variable v) : tokens) = Just (Var v, tokens)
parseTerm tokens =
  case parseExpression tokens of
    Just (TempExpr e t, more) -> Just (Function e t, more)
    Just (TempVar v, more) -> Just (Var v, more)
    _ -> Nothing

parseTerms :: [Token] -> [Term] -> Maybe ([Term], [Token])
parseTerms tokens terms =
  case parseTerm tokens of
    Just (t, Comma:more) -> parseTerms more (terms ++ [t])
    Just (t, more) -> Just (terms ++ [t], more)
    _ -> Nothing

parsePredicateExpression :: [Token] -> Maybe (Formula, [Token])
parsePredicateExpression tokens =
  case parseExpression tokens of
    Just (TempExpr e t, more) -> Just (Predicate e t, more)
    _ -> Nothing

parseNegation :: [Token] -> Maybe (Formula, [Token])
parseNegation tokens =
  case parseFormula tokens of
    Just(f, Rparen:more) -> Just(F.Negation f, more)
    _ -> Nothing

parseConjunction :: [Token] -> [Formula] -> Maybe (Formula, [Token])
parseConjunction tokens formulas =
  case parseFormula tokens of
    Just (f, Comma:more) -> parseConjunction more (formulas ++ [f])
    Just (f, Rparen:more) -> Just(F.Conj (formulas ++ [f]), more)
    _ -> Nothing

parseDisjunction :: [Token] -> [Formula] -> Maybe (Formula, [Token])
parseDisjunction tokens formulas =
  case parseFormula tokens of
    Just (f, Comma:more) -> parseDisjunction more (formulas ++ [f])
    Just (f, Rparen:more) -> Just(F.Disj (formulas ++ [f]), more)
    _ -> Nothing

parseImplication :: [Token] -> Maybe (Formula, [Token])
parseImplication tokens =
  case parseFormula (tokens) of
    Just(f1, Comma:m1) -> case parseFormula(m1) of
      Just (f2, Rparen:m2) -> Just (F.Implies f1 f2, m2)
      _ -> Nothing
    _ -> Nothing

parseEquivalence :: [Token] -> Maybe (Formula, [Token])
parseEquivalence tokens =
  case parseFormula (tokens) of
    Just(f1, Comma:m1) -> case parseFormula(m1) of
      Just (f2, Rparen:m2) -> Just (F.Equiv f1 f2, m2)
      _ -> Nothing
    _ -> Nothing

parseExistentialQuantifier :: [Token] -> Maybe(Formula, [Token])
parseExistentialQuantifier ((Variable v) : Comma : tokens) =
  case parseFormula(tokens) of
    Just (f, Rparen:more) -> Just(F.Exists v f, more)
    _ -> Nothing
parseExistentialQuantifier tokens = Nothing

parseUniversalQuantifier :: [Token] -> Maybe(Formula, [Token])
parseUniversalQuantifier ((Variable v) : Comma : tokens) =
  case parseFormula(tokens) of
    Just (f, Rparen:more) -> Just(F.ForAll v f, more)
    _ -> Nothing
parseUniversalQuantifier tokens = Nothing

parseFormula :: [Token] -> Maybe (Formula, [Token])
parseFormula (T.Not : Lparen : tokens) = parseNegation tokens
parseFormula (T.And : Lparen : tokens) = parseConjunction tokens []
parseFormula (T.Or : Lparen : tokens) = parseDisjunction tokens []
parseFormula (T.Impl : Lparen : tokens) = parseImplication tokens
parseFormula (T.Equ : Lparen : tokens) = parseEquivalence tokens
parseFormula (T.ExisQ : Lparen : tokens) = parseExistentialQuantifier tokens
parseFormula (T.UnivQ : Lparen : tokens) = parseUniversalQuantifier tokens
parseFormula tokens = parsePredicateExpression tokens

data TE = TempExpr Name [Term] | TempVar Identifier | TempFormula Formula
parseExpression :: [Token] -> Maybe (TE, [Token])
parseExpression ((Id s) : Lparen : tokens) =
  case parseTerms tokens [] of
    Just (t, Rparen:more) -> Just (TempExpr s t, more)
    _ -> Just (TempExpr s [], tokens)
parseExpression ((Id s) : tokens) = Just (TempExpr s [], tokens)
parseExpression ((Variable v) : tokens) = Just (TempVar v, tokens)
parseExpression tokens = Nothing
