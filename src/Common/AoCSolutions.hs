module Common.AoCSolutions
  ( printSolutions
  , printTestSolutions
  , AoCSolution(MkAoCSolution)
  ) where

import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.Except (ExceptT, except, runExceptT,
                                             withExceptT)
import           GHC.Base                   (Any)
import           Text.Trifecta              (ErrInfo, Parser, Result,
                                             foldResult)
import           Text.Trifecta.Parser       (parseString)
import           Web.AoCUtils               (ConfigError, getPuzzleInput,
                                             getTestPuzzleInput)
import System.TimeIt (timeIt)

data AoCSolution a b =
  MkAoCSolution
    { _parser   :: Parser a
    , _solution :: a -> b
    }

type GetPuzzleInput = Integer -> ExceptT ConfigError IO String

data SolutionError
  = MkConfigError ConfigError
  | MKParseError ErrInfo
  deriving (Show)

printSolutions' ::
     (Show b) => Integer -> Char -> GetPuzzleInput -> AoCSolution a b -> IO ()
printSolutions' day part puzzleInputFun (MkAoCSolution parser part1) = do
  result <-
    runExceptT $ do
      input <- withExceptT MkConfigError $ puzzleInputFun day
      parsed <- except $ resultToEither $ parseString parser mempty input
      lift $ do
        putStr (show day ++ [part] ++ ": ")
        print $ part1 parsed
  either print return result

printSolutions :: (Show b) => Integer -> Char -> AoCSolution a b -> IO ()
printSolutions day part = printSolutions' day part getPuzzleInput

printTestSolutions :: (Show b) => Integer -> Char -> AoCSolution a b -> IO ()
printTestSolutions day part = printSolutions' day part getTestPuzzleInput

resultToEither :: Result a -> Either SolutionError a
resultToEither = foldResult (Left . MKParseError) Right
