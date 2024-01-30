module Main where

import BasicPrelude
import Control.Exception (try)
import Control.Monad.Loops (unfoldM_)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Minsk.SyntaxNode (SyntaxNode (SyntaxNode), pprint)
import qualified Minsk.SyntaxTree as SyntaxTree

done :: IO (Maybe ())
done = return Nothing

next :: IO (Maybe ())
next = return $ Just ()

main :: IO ()
main = unfoldM_ do
  putStr "> "
  hFlush stdout
  input <- try getLine
  case input of
    Left e ->
      if isEOFError e
        then do
          putStrLn ""
          done
        else ioError e
    Right line -> do
      let ast = SyntaxTree.parse line
      mapM_ print ast.diagnostics
      pprint stdout $ SyntaxNode ast.root
      next
