module Util where

import Happy.Middleend(Lr1State)
import Happy.Core.Grammar
import Happy.Core.Tables

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
  
-- | Common
multi :: String -> [String] -> String
multi del = foldr (\accu s -> accu ++ del ++ s) ""

multiLines :: [String] -> String
multiLines = multi "\n" 

multiWords :: [String] -> String
multiWords = multi " " 

