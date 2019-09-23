{-# LANGUAGE FlexibleInstances, RankNTypes, RecordWildCards, ScopedTypeVariables #-}

module Main where

import qualified Language.Dart.Grammar as Grammar
import Language.Dart.Pretty (Pretty, prettyPrint)

import Control.Monad
import Data.Data (Data)
import Data.Functor.Identity (Identity)
import Data.Functor.Compose (getCompose)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Options.Applicative
import Text.Grampa (Grammar, ParseResults, fixGrammar, parseComplete, failureDescription)
import qualified Text.Grampa.ContextFree.LeftRecursive as LeftRecursive
import ReprTree
import System.FilePath (FilePath)

data GrammarMode = CompilationUnitMode | DeclarationMode | ExpressionMode
    deriving Show

data Output = Plain | Pretty | Tree
            deriving Show

data Opts = Opts
    { optsMode        :: GrammarMode
    , optsIndex       :: Int
    , optsOutput      :: Output
    , optsFile        :: Maybe FilePath
    } deriving Show

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> p)
        ( fullDesc
       <> progDesc "Parse a Dart file, or parse interactively"
       <> header "Dart parser")

    p :: Parser Opts
    p = Opts
        <$> (mode <|> pure CompilationUnitMode)
        <*> (option auto (long "index" <> help "Index of ambiguous parse" <> showDefault <> value 0 <> metavar "INT"))
        <*> (flag' Pretty (long "pretty" <> help "Pretty-print output")
             <|> flag' Tree (long "tree" <> help "Print the output as an abstract syntax tree")
             <|> pure Plain)
        <*> optional (strArgument
            ( metavar "FILE"
              <> help "Dart file to parse"))

    mode :: Parser GrammarMode
    mode = CompilationUnitMode <$ switch (long "compilation-unit")
       <|> DeclarationMode     <$ switch (long "statement")
       <|> ExpressionMode      <$ switch (long "expression")

main' :: Opts -> IO ()
main' opts@Opts{..} =
    case optsFile of
        Just file -> (if file == "-" then getContents else readFile file)
                     >>= case optsMode
                         of CompilationUnitMode -> go Grammar.compilationUnit file

        Nothing ->
            forever $
            getLine >>=
            case optsMode of
                CompilationUnitMode -> go Grammar.compilationUnit "<stdin>"
                DeclarationMode     -> go Grammar.statement "<stdin>"
                ExpressionMode      -> go Grammar.expression "<stdin>"
  where
    go :: (Show f, Data f, Pretty f) => (forall p. Grammar.Grammar p -> p f) -> String -> String -> IO ()
    go production filename contents =
       case getCompose (production $ parseComplete (fixGrammar Grammar.grammar) contents)
       of Right [x] -> succeed optsOutput x
          Right l -> putStrLn ("Ambiguous: " ++ show optsIndex ++ "/" ++ show (length l) ++ " parses")
                     >> succeed optsOutput (l !! optsIndex)
          Left err -> putStrLn (failureDescription contents err 3)

succeed Pretty = putStrLn . prettyPrint
succeed Tree = putStrLn . reprTreeString
succeed Plain = print
