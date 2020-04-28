
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Monad.State.Strict
import           Prelude                    hiding (init)
import           System.Console.Repline
import           System.Directory
import           System.FilePath.Posix
import           System.IO

import           Data.List                  (intercalate, isPrefixOf, sortBy)
import           Data.Maybe                 (fromMaybe, fromJust)
import           Data.Ord                   (comparing)
import           Text.Printf                (printf)

import           Grammar                    (Grammar (..), grammar0, grammar1,
                                             grammar2, grammar3, recognize)

type IState = Maybe String
type Repl a = HaskelineT (StateT IState IO) a

cmd :: String -> Repl ()
cmd input = liftIO $ print input

opts :: [(String, [String] -> Repl ())]
opts = [
    ("load", load)
  , ("show", showFile)
  , ("show_me_what_you_got", showGrammars)
  , ("parenthesis", recognizeParentheses)
  , ("recognize", recognizeParenthesesFromFile)
  , ("expression", recognizeExpression)
  , ("quit", quit)
  , ("help", help)
  ]

load :: [String] -> Repl ()
load [name] = do
  fileExists <- liftIO $ doesFileExist name
  if fileExists
  then do
    file <- liftIO $ readFile name
    put $ Just file
  else
    liftIO $ putStrLn $ printf "File does not exist: %s" name
load _ =
  liftIO $ putStrLn "Please only give me one file name "

showFile :: [String] -> Repl ()
showFile _ = do
  file <- get
  liftIO $ putStrLn $ fromMaybe "No file loaded" file

showGrammars :: [String] -> Repl ()
showGrammars _ = do
    liftIO $ putStrLn "\nAll I have are these grammars\n\n========================================\nFirst:\n========================================\n"
    liftIO $ print grammar0
    liftIO $ putStrLn "\n========================================\nSecond:\n========================================\n"
    liftIO $ print grammar1
    liftIO $ putStrLn "\n========================================\nThird\n========================================\n"
    liftIO $ print grammar2
    liftIO $ putStrLn "\n========================================\nFourth\n========================================\n"
    liftIO $ print grammar3

recognizeParenthesesFromFile :: [String] -> Repl ()
recognizeParenthesesFromFile _ = do
  file <- get
  maybe (liftIO $ putStrLn "No file loaded") (recognizeParentheses . lines) file


recognizeParentheses :: [String] -> Repl ()
recognizeParentheses input =
    mapM_ (liftIO . checkString) input
  where
    checkString str =
      putStrLn $ message str (recognize grammar3 str)

message :: String -> Bool -> String
message str b = printf "%s %s be derived"
                       str
                       (if b then "can" else "can't")

recognizeExpression :: [String] -> Repl ()
recognizeExpression input =
    mapM_ (liftIO . checkString) input
  where
    checkString str =
      putStrLn $ message str (recognize grammar2 str)


quit :: [String] -> Repl ()
quit _ = abort

help :: [String] -> Repl ()
help _ = liftIO $ putStrLn helpMessage

helpMessage =
  printf  "\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n"
          ":load filename\t\tTo load the given filename"
          ":show\t\t\tTo print the loaded file"
          ":show_me_what_you_got\tTo print all grammars"
          ":parenthesis x1 .. xn\tTo check if xi can be recognized by the fourth grammar"
          ":expression x1 .. xn\tTo check if xi can be recognized by the third grammar"
          ":recognize\t\tTo check if the lines in the input file can be recognized \n\t\t\tby the forth grammar"
          ":quit\t\t\tTo quit"
          ":help\t\t\tTo print this message"


init :: Repl ()
init =
  liftIO $ putStrLn ("Welcome!\n" ++ helpMessage)

-- Completion
comp :: Monad m => WordCompleter m
comp = listWordCompleter $ map (':' :) $ map fst opts

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load", fileCompleter)
  ]

repl = flip evalStateT Nothing
     $ evalRepl (pure "REPL> ") cmd opts (Just ':') (Prefix (wordCompleter comp) defaultMatcher) init

main = repl
