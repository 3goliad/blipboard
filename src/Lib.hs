{-# LANGUAGE DeriveGeneric #-}
module Lib (loadConfig, generateImport) where

import Protolude

import qualified Data.Csv as Csv
import Data.UUID (UUID)
import Data.Tree (Tree)
import System.Random (RandomGen, getStdGen, randoms)
import Time.Types (Date)

data Entity = Entity { externalId :: UUID
                     , name :: Text
                     } deriving (Show)

mkEntity prefix uuid =
  Entity { externalId = uuid
         , name = prefix <> show uuid
         }

mkWorker = mkEntity "Worker "
mkOrg = mkEntity "Shop "

type Org = Tree Entity

type Department = [Entity]

genEmployers = undefined

data Employment = Employment { hired :: Date, fired :: Maybe Date }

data Sim g = Sim { day :: Date
                 , workers :: [Entity]
                 , employers :: [Department]
                 , gen :: g
                 } deriving (Generic, Show)

initSim :: RandomGen g => g -> Config -> Sim g
initSim g (Config pop sd) = Sim
  { day = sd
  , workers = take pop $ mkWorker <$> randoms g
  , employers = genEmployers pop g
  , gen = g
  }

stepSim :: RandomGen g => Sim g -> Sim g
stepSim = undefined

printSim :: Sim g -> [Row]
printSim = undefined

data Row = Row Entity (Maybe Employment)

instance Csv.ToRecord Row where
  toRecord (Row ent emp) = undefined

data Config = Config { population :: Int
                     , startDate :: Date
                     }

loadConfig :: Int -> Date -> Config
loadConfig = Config

generateImport :: Config -> IO ()
generateImport config = do
  g <- getStdGen
  let
    sim = initSim g config
  print . Csv.encode $ printSim $ stepSim sim
