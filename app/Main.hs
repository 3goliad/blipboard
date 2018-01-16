{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Dhall
import qualified Data.Text as T
import Data.Time.Format (ParseTime, TimeLocale, parseTimeOrError, defaultTimeLocale, buildTime)
import Data.Time.Calendar (Day)

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
    sd = parseTimeOrError True defaultTimeLocale "%F" $ T.unpack $ startDate config
    in generateImport $ loadConfig pop sd
