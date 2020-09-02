--
-- Maxim Puchkov
-- 301314335
-- CMPT 384 Assignment 3
--
-- First-order logic
--

module Main where

import System.Environment
import System.IO (stdout,stderr,hPutStr,hPutStrLn)

-- Part 1. Translate Formula data type to ClauseFormEquivalent data type

import FormulaTokenizer
import FormulaParser
import Formula
import ClauseFormEquivalent






-- Part 2. Formulas representing Puzzle 1.
-- https://coursys.sfu.ca/2018fa-cmpt-384-d1/pages/A4
-- Actual order: Swim(Qin, Fariba, Mary, Paul), Biking(Paul, Mary, Qin, Fariba)

f1 = ForAll "Race" (Negation (Predicate "first" [Var "Race",Var "Fariba"])) -- 1
f2 = ForAll "Person" (Implies (Predicate "first" [Var "Swim",Var "Person"]) (Predicate "third" [Var "Bike",Var "Person"])) -- 2
f3 = Predicate "better" [Var "Swim",Var "Fariba",Var "Mary"] -- 3
f4 = Predicate "better" [Var "Bike",Var "Mary",Var "Fariba"]
f5 = ForAll "Race" (Negation (Predicate "last" [Var "Race",Var "Mary"])) -- 4
f6 = Predicate "first" [Var "Bike",Var "Paul"] -- 5
f7 = Predicate "better" [Var "Swim",Var "Qin",Var "Paul"]







-- Part 3. Formulas as a single Conjunction.
-- 5 clauses will have at least one positive literal

allFormulas = Conj[f1, f2, f3, f4, f5, f6, f7]

allFormulasCNF = conjunctiveNormalForm allFormulas
allFormulasCFE = convertToClauseFormEquivalent allFormulas
positiveLiteralCount = countPosLitClauses allFormulasCFE






main = do
  hPutStrLn stdout ("")
  hPutStrLn stdout ("")

  hPutStrLn stdout ("Original representation of formulas:")
  hPutStrLn stdout ("____________________________________")
  hPutStrLn stdout (show allFormulas)
  hPutStrLn stdout ("")
  hPutStrLn stdout ("")

  hPutStrLn stdout ("Conjunctive Normal Form representation:")
  hPutStrLn stdout ("_______________________________________")
  hPutStrLn stdout (show allFormulasCNF)
  hPutStrLn stdout ("")
  hPutStrLn stdout ("")

  hPutStrLn stdout ("Clause-Form Equivalent representation:")
  hPutStrLn stdout ("______________________________________")
  hPutStrLn stdout (show allFormulasCFE)
  hPutStrLn stdout ("")
  hPutStrLn stdout ("")

  hPutStrLn stdout ("Number of clauses having at least one positive literal:")
  hPutStrLn stdout ("_______________________________________________________")
  hPutStrLn stdout (show positiveLiteralCount)
  hPutStrLn stdout ("")
  hPutStrLn stdout ("")
