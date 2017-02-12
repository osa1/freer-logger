{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Monad.Freer
import           Control.Monad.Freer.Logger
import           Control.Monad.Writer
import qualified Data.ByteString.Char8      as BS8
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           System.Exit                (exitFailure)
--------------------------------------------------------------------------------

f1 :: Member Logger r => String -> Eff r ()
f1 input = do
    $(logDebug) ("Input: " <> T.pack (show input))

    $(logWarn) "This is a warning"

    $(logError) "This is an error"

    return ()

logWriter :: Loc -> LogSource -> LogLevel -> LogStr -> Writer [LogStr] ()
logWriter loc _src _lvl str =
    tell [ toLogStr (show (loc_start loc)) <> toLogStr (" " :: String) <> toLogStr (show (loc_end loc)) <> toLogStr (" " :: String)  <> str ]

main :: IO ()
main = do
    let
      ret :: Eff '[Writer [LogStr]] ()
      ret = runLogger logWriter (f1 "logging effect works")

      ((), logs) = runWriter (runM ret)

    mapM_ (BS8.putStrLn . fromLogStr) logs

    assert $ ["(20,7) (20,15) Input: \"logging effect works\"","(22,7) (22,14) This is a warning","(24,7) (24,15) This is an error"]
             ==
             map fromLogStr logs

assert :: Bool -> IO ()
assert True  = return ()
assert False = putStrLn "Test failed!" >> exitFailure
