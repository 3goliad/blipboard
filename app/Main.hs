{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Dhall
import qualified Text.Read as TR (read)
import qualified Data.Text as T
import Time.Types (Date)

import Lib (loadConfig, generateImport)

data Config = Config { population :: Integer
                     , startDate :: T.Text
                     } deriving (Dhall.Generic, Show)

instance Dhall.Interpret Config

main :: IO ()
main = do
  config <- Dhall.input Dhall.auto "./config"
  let
    pop = fromIntegral $ population config
    sd = TR.read . T.unpack $ startDate config
    in generateImport $ loadConfig pop sd
