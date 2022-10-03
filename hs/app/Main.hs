module Main where

import Control.Exception (try)
import Control.Exception.Base (Exception)
import qualified Data.Text as Text
import GHC.IO.Exception (IOException)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import qualified Minsk.Lexer as Lexer
import System.IO.Error (isEOFError)

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
                let tokens = Lexer.allTokens $ Text.pack line
                 in print tokens
                loop ()