import System.IO (stderr, hPutStr, hPutStrLn)
import System.Exit (exitFailure)
import System.Environment (getArgs)

import Control.Monad.Except (runExceptT)

import Latte.Abs
import Latte.ErrM
import Latte.Par

import Frontend (runStaticAnalysis)


latteLexer = myLexer

staticAnalysis :: String -> IO ()
staticAnalysis input = do
    case pProgram (latteLexer input) of
        Bad _ -> do
            hPutStrLn stderr "Failed to parse input:"
            hPutStrLn stderr input
            exitFailure
        Ok programTree -> do
            staticAnalysisResult <- runExceptT (runStaticAnalysis tree)
            case staticAnalysisResult of
                Left errorMessage -> do
                    hPutStrLn stderr "Error: static analysis exception"
                    hPutStrLn stderr errorMessage
                    exitFailure
                Right _ -> do
                    hPutStrLn stderr "OK"


    putStrLn "Hello world!"

main :: IO ()
main = do
    programArguments <- getArgs
    case programArguments of
        [file] -> readFile file >>= staticAnalysis
        []     -> getContents >>= staticAnalysis
        _      -> do
            putStrLn "Correct usage: ./latc <file>"
            exitFailure
