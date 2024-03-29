module Main where

import BasicPrelude
import Control.Exception (try)
import Formatting (fprintLn, text)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import qualified Minsk.AST as AST
import Minsk.Evaluator (evaluate)
import qualified Minsk.SyntaxTree as SyntaxTree
import System.Console.ANSI
  ( Color (Black, Red),
    ColorIntensity (Dull, Vivid),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor),
    setSGR,
  )

unfoldM_ :: (Monad m) => (s -> m (Maybe s)) -> s -> m ()
unfoldM_ f s = f s >>= maybe (return ()) (unfoldM_ f)

data State = State {showTree :: Bool}

repl :: State -> IO (Maybe State)
repl state = do
  putStr "> "
  hFlush stdout
  input <- try getLine
  case input of
    Left e ->
      if isEOFError e
        then do
          putStrLn ""
          return Nothing
        else ioError e
    Right line -> case line of
      "#showTree" -> do
        let showTree' = not state.showTree
        fprintLn text $ if showTree' then "Showing parse trees." else "Not showing parse trees."
        return $ Just state {showTree = showTree'}
      _ -> do
        let ast = SyntaxTree.parse line
        case ast.diagnostics of
          [] -> do
            when state.showTree do
              setSGR [SetColor Foreground Vivid Black]
              AST.pprint stdout $ AST.NExpression () ast.root
              hFlush stdout
              setSGR [Reset]
            case evaluate ast.root of
              Left err -> do
                setSGR [SetColor Foreground Dull Red]
                putStrLn err
                hFlush stdout
                setSGR [Reset]
              Right result -> do
                print result
          diagnostics -> do
            setSGR [SetColor Foreground Dull Red]
            mapM_ print diagnostics
            hFlush stdout
            setSGR [Reset]
        return $ Just state

initialState :: State
initialState = State {showTree = False}

main :: IO ()
main = unfoldM_ repl initialState
