module Util where

import Happy.Middleend(Lr1State)
import Happy.Core.Grammar
import Happy.Core.Tables

import qualified AutomatonType as AT

import Data.Array

showProduction g (Production lhs rhss (n,is) prio) =
  token_names g ! lhs ++ " -> "
  ++ multiWords (map (token_names g !) rhss)
  ++ " " {- ++ show (n, is) -} ++   -- code!!
  showPriority prio

showProductions g = 
  multiLines $
    foldl
      (\ss (i,s) -> ss ++ [show i ++ ": " ++ s])
      []
      (zip [0..] (map (showProduction g) (productions g)))

------------------------------------------------------
-- | AT.ProdRules
------------------------------------------------------
prProdRule g (Production lhs rhss _ _) =
  (token_names g ! lhs, map (token_names g !) rhss)

prProdRules g =
  multi ",\n" $
    map (\(x,y) -> show (x, showNoQuotes y)) $
      map (prProdRule g) (productions g)

showNoQuotes [] = []
showNoQuotes (symbol:symbols) = showNoQuote symbol : showNoQuotes symbols

showNoQuote symbol = 
  if head symbol == '\'' && last symbol == '\''
  then tail (init symbol) else symbol
  
------------------------------------------------------

showTerminals g =
  multiLines $
    foldl
      (\ss (i,s) -> ss ++ [show i ++ " : " ++ s ++ " : " ++ getToken i])
      []
      [ (terminal, (token_names g) ! terminal) | terminal <- terminals g ]
  where
    getToken terminal =
      case [ s | (n,s) <- token_specs g, n == terminal ] of
        [] -> "???"
        s:_ -> s

prFromToken g =
  multiLines $
    foldl
      (\ss (i,s) -> ss ++ ["fromToken (" ++ getToken i ++ ") = " ++ s])
      []
      [ (terminal, (token_names g) ! terminal) | terminal <- terminals g ]
  where
    getToken terminal =
      case [ s | (n,s) <- token_specs g, n == terminal ] of
        [] -> "???"
        s:_ -> s
  
showToken_Specs g = 
  multiLines $ map (\(i,s) -> show i ++ " : " ++ s) $ token_specs g

showStart g (funName, i, non_terminal, isPartial) =
  token_names g ! non_terminal
  
showStarts g starts =
  multiLines $ map (showStart g) starts
  
showGrammar g =
  multiLines
  [ "Production rules:"
  ,  showProductions g
  -- , showToken_Specs g
  -- , "terminals: " ++
  --     show [ (terminal, (token_names g) ! terminal)
  --          | terminal <- terminals g ]
  -- , "nonterminals: " ++
  --     show [ (non_terminal, (token_names g) ! non_terminal)
  --          | non_terminal <- non_terminals g ]
  , "Start symbols:"
  , showStarts g (starts g)
  ]

-- | Tables

showActionTable :: Grammar -> [Lr1State] -> ActionTable -> String
showActionTable g _lr1States actionTable =
  let lr1States = map fst (assocs actionTable) in
  multiLines
  [ "s" ++ show s ++ " " ++
    token_names g ! terminal ++ " " ++
    showAction action
  | s <- lr1States, let arr = actionTable ! s, (terminal, action) <- assocs arr, relevant action]

  where
    relevant (LR'Shift _ _) = True
    relevant (LR'Reduce _ _) = True
    relevant (LR'Accept) = True
    relevant (LR'Multiple _ _) = True
    relevant _ = False

showAction (LR'Shift i prio) = "LR'Shift s" ++ show i ++ " " ++ showPriority prio
showAction (LR'Reduce i prio) = "LR'Reduce " ++ show i ++ " " ++ showPriority prio
showAction (LR'Accept) = show LR'Accept
showAction (LR'Multiple actions action) = show $ LR'Multiple actions action
showAction _ = ""

showPriority No = ""
showPriority prio = "[" ++ show prio ++ "]"

------------------------------------------------------
-- | AT.ActionTable
------------------------------------------------------

prActionTable :: Grammar -> [Lr1State] -> ActionTable -> String
prActionTable g _lr1States actionTable =
  let lr1States = map fst (assocs actionTable) in
  multi ",\n" $
  map show $
  [ ((s, showNoQuote $ token_names g ! terminal), prAction action)
  | s <- lr1States, let arr = actionTable ! s, (terminal, action) <- assocs arr, relevant action]

  where
    relevant (LR'Shift _ _) = True
    relevant (LR'Reduce _ _) = True
    relevant (LR'Accept) = True
    relevant (LR'Multiple _ _) = True
    relevant _ = False

prAction (LR'Shift i prio) = AT.Shift i
prAction (LR'Reduce i prio) = AT.Reduce i
prAction (LR'Accept) = AT.Accept
prAction (LR'Multiple actions action) = error $ "unexpected: prActionTable: LR'Multiple"
prAction _ = error $ "unexpected: prActionTable: _"

------------------------------------------------------

  
showGotoTable :: Grammar -> [Lr1State] -> GotoTable -> String
showGotoTable g _lr1States gotoTable =
  let lr1States = map fst (assocs gotoTable) in
  multiLines
  [ "s" ++ show s ++ " " ++
    token_names g ! non_terminal ++ " " ++
    case goto of
      Goto i -> "s" ++ show i
      NoGoto -> show "nowhere"
    | s <- lr1States, let arr = gotoTable ! s, (non_terminal, goto) <- assocs arr, relevant goto ]
  
  where
    relevant (Goto _) = True
    relevant (NoGoto) = False

------------------------------------------------------
-- | AT.GotoTable
------------------------------------------------------

prGotoTable :: Grammar -> [Lr1State] -> GotoTable -> String
prGotoTable g _lr1States gotoTable =
  let lr1States = map fst (assocs gotoTable) in
  multi ",\n" $
  map show $
  [ ((s, token_names g ! non_terminal), 
    case goto of
      Goto i -> i
      NoGoto -> error $ "unexpected: prGotoTable: NoGoto")
    | s <- lr1States, let arr = gotoTable ! s, (non_terminal, goto) <- assocs arr, relevant goto ]
  
  where
    relevant (Goto _) = True
    relevant (NoGoto) = False

------------------------------------------------------
    
  
-- | Common
multi :: String -> [String] -> String
multi del = foldr (\accu s -> accu ++ del ++ s) ""

multiLines :: [String] -> String
multiLines = multi "\n" 

multiWords :: [String] -> String
multiWords = multi " " 

