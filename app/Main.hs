module Main where

import qualified Happy.Frontend.CLI as FE
import qualified Happy.Middleend.CLI as ME
import Happy.Core.Tables (LRAction(..), ActionTable, Goto(..), GotoTable)
import Happy.Core.Grammar (Grammar(..))

import Util

{-

> data LRAction = LR'Shift Int Priority -- state number and priority
>               | LR'Reduce Int Priority-- rule no and priority
>               | LR'Accept             -- :-)
>               | LR'Fail               -- :-(
>               | LR'MustFail           -- :-(
>               | LR'Multiple [LRAction] LRAction       -- conflict
>       deriving (Eq, Show)

> type ActionTable = Array Int{-state-} (Array Int{-terminal#-} LRAction)

> data Goto = Goto Int | NoGoto
>       deriving(Eq, Show)

> type GotoTable = Array Int{-state-} (Array Int{-nonterminal #-} Goto)

---

> data Grammar
>       = Grammar {
>               productions       :: [Production],
>               lookupProdNo      :: Int -> Production,
>               lookupProdsOfName :: Name -> [Int],
>               token_specs       :: [(Name,String)],                -- VARID, L _ (ITvarid _)
>               terminals         :: [Name],
>               non_terminals     :: [Name],
>               starts            :: [(String,Name,Name,Bool)],
>               types             :: Array Int (Maybe String),
>               token_names       :: Array Int String,               -- 436 : VARID
>               first_nonterm     :: Name,
>               first_term        :: Name,
>               eof_term          :: Name,
>               priorities        :: [(Name,Priority)],
>               token_type        :: String,
>               imported_identity :: Bool,
>               monad             :: (Bool,String,String,String,String),
>               expect            :: Maybe Int,
>               attributes        :: [(String,String)],
>               attributetype     :: String,
>               lexer             :: Maybe (String,String),
>               error_handler     :: Maybe String,
>               error_sig         :: ErrorHandlerType,
>               hd                :: Maybe String,
>               tl                :: Maybe String
>       }

> data Production
>       = Production Name [Name] (String,[Int]) Priority
>       deriving (Show)

> data Assoc = LeftAssoc | RightAssoc | None
>       deriving Show

> data Priority = No | Prio Assoc Int | PrioLowest
>       deriving Show

-}


main :: IO ()
main = do
  let fileName = "Parser.y"
  let baseName = "Parser"
        
  e <- FE.parseAndRun [] fileName baseName
  case e of
    Left err -> putStrLn err
    Right grammar -> do
      putStrLn "Grammar:"
      putStrLn $ showGrammar grammar
      
      (actionTable, gotoTable, lr1States, unused_rules)
        <- ME.parseAndRun [] fileName baseName grammar
--      putStrLn $ show lr1States
      putStrLn "Action table:"
      putStrLn $ showActionTable grammar lr1States actionTable
      putStrLn "Goto table:"
      putStrLn $ showGotoTable grammar lr1States gotoTable
      putStrLn "Token spec:"
      putStrLn $ showToken_Specs grammar
      putStrLn "Terminals:"
      putStrLn $ showTerminals grammar

      -- For associating lexer with parser
      -- putStrLn "fromToken:"
      -- putStrLn $ prFromToken grammar

      -- For parser
      putStrLn "ProdRules:"
      putStrLn $ prProdRules grammar

      putStrLn "ActionTable:"
      putStrLn $ prActionTable grammar lr1States actionTable

      putStrLn "GotoTable:"
      putStrLn $ prGotoTable grammar lr1States gotoTable
