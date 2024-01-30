module Main where

import BasicPrelude
import Control.Exception (try)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import qualified Minsk.SyntaxTree as SyntaxTree

main :: IO ()
main = loop ()
  where
    loop :: () -> IO ()
    loop () = do
      putStr "> "
      hFlush stdout
      input <- try getLine
      case input of
        Left e ->
          if isEOFError e
            then do
              putStrLn ""
              return ()
            else ioError e
        Right line -> do
          let ast = SyntaxTree.parse line
          mapM_ print ast.diagnostics
          loop ()
