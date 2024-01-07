import System.IO (stderr, putStrLn, hPutStr, hPutStrLn)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.FilePath.Posix (replaceExtension, takeBaseName)

import Control.Monad.Except (runExceptT)

import Latte.Abs
import Latte.ErrM
import Latte.Par

import Frontend (runStaticAnalysis)
import Backend (runCompiler)

latteLexer = myLexer

staticAnalysis :: String -> IO String
staticAnalysis input = do
    case pProgram (latteLexer input) of
        Bad _ -> do
            hPutStrLn stderr "Failed to parse input:"
            hPutStrLn stderr input
            exitFailure
        Ok programTree -> do
            staticAnalysisResult <- runExceptT (runStaticAnalysis programTree)
            case staticAnalysisResult of
                Left errorMessage -> do
                    hPutStrLn stderr "Error: static analysis exception"
                    hPutStrLn stderr errorMessage
                    exitFailure
                Right _ -> do
                    compilerResult <- runExceptT (runCompiler programTree)
                    case compilerResult of
                        Left errorMessage -> do
                            hPutStrLn stderr "Error: compilation error"
                            hPutStrLn stderr errorMessage
                            exitFailure
                        Right generatedCode -> do
                            hPutStrLn stderr "OK\n"
                            return generatedCode

writeCodeToFile :: String -> String -> IO ()
writeCodeToFile file code = do
    let newFileName = replaceExtension file ".s"
    writeFile newFileName code
    return ()

main :: IO ()
main = do
    programArguments <- getArgs
    case programArguments of
        [file] -> readFile file >>= staticAnalysis >>= writeCodeToFile file
        -- []     -> getContents >>= staticAnalysis
        _      -> do
            putStrLn "Correct usage: ./latc <file>"
            exitFailure
