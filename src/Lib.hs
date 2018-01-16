{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib (loadConfig, generateImport) where

import           Protolude

import Data.Csv (ToNamedRecord, ToField, toField, namedRecord, toNamedRecord, encodeByName, header, (.=))
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.String (IsString, fromString)
import           Data.Tree (Tree, unfoldForestM, drawForest, levels, foldTree)
import           Data.UUID (UUID)
import           System.Random (RandomGen, getStdGen, random, randoms, randomIO)
import           Data.Time.Calendar (Day)
import           Data.Time.Calendar.Julian (showJulian)

data Entity = Entity { externalId :: UUID
                     , name :: Text
                     , lastName :: Text
                     } deriving (Show, Generic)

mkEntity prefix uuid =
  Entity { externalId = uuid
         , name = prefix <> show uuid
         , lastName = "LastName"
         }

mkWorker = mkEntity "Worker "
mkOrg = mkEntity "Shop "

type Org = Tree Entity

type Department = [Entity]

orgsInTree :: [Int] -> Int
orgsInTree splay = sum $ foldl branch [] splay
  where
    branch (prev:rest) next  = next * prev : prev : rest
    branch [] next = next : []

empTree :: Int -> [Int]
empTree pop = let
  p = divUp pop 100
  in if p > 41 then
       [divUp p 41, 8, 4]
     else if p > 17 then
            [divUp p 17, 4, 3]
          else
            [divUp p 5, 4]
  where
    divUp :: Int -> Int -> Int
    divUp a b =
      case quotRem a b of
        (amt, 0) -> amt
        (amt, rem) -> amt + 1

genEmployers :: Int -> IO [Org]
genEmployers pop = unfoldForestM f (dupeRest (empTree pop))
  where
    dupeRest l = case l of
                   [] -> []
                   (x : xs) -> replicate x xs
    f :: [Int] -> IO (Entity, [[Int]])
    f l = do
      uuid <- randomIO
      return (mkOrg uuid, dupeRest l)

teEmp :: IO ()
teEmp = do
  emps <- genEmployers 1500
  putStrLn $ drawForest (fmap (T.unpack . name) <$> emps)
  print $ fmap length <$> paths <$> emps


paths :: Tree a -> [[a]]
paths = foldTree pushPath
  where
    pushPath label subtrees = [label] : prependToPaths label subtrees
    prependToPaths :: a -> [[[a]]] -> [[a]]
    prependToPaths label subtrees = do
      subtree <- subtrees
      subtree >>= return . (:) label

data Employment = Employment { employer :: Department
                             , hired :: Maybe Day
                             , fired :: Maybe Day
                             } deriving (Generic, Show)
instance ToNamedRecord (Maybe Employment) where
  toNamedRecord (Just (Employment employer hired fired)) =
    namedRecord [ "dept" .= deptName employer
                , "hired" .= hired
                , "fired" .= fired
                ]
  toNamedRecord Nothing =
    namedRecord [ "dept" .= ("" :: ByteString)
                , "hired" .= ("" :: ByteString)
                , "fired" .= ("" :: ByteString)
                ]

genEmployments :: Day -> Int -> [Department] -> [Maybe Employment]
genEmployments sd pop emps = unemployed <> fired <> employed
  where
    nUn = ceiling $ (toRational pop) * 0.3
    nFired = ceiling $ (toRational pop) * 0.1
    nRest = ceiling $ (toRational pop)* 0.6
    unemployed = take nUn (repeat Nothing)
    fired = take nFired [Just $ Employment employer Nothing (Just sd) | employer <- reverse emps]
    employed = take nRest [Just $ Employment employer Nothing Nothing | employer <- emps]

data Sim = Sim { day :: Day
               , workers :: [(Entity, Maybe Employment)]
               , employers :: [Department]
               } deriving (Generic, Show)

initSim :: Config -> IO Sim
initSim (Config pop sd) = do
  emps <- genEmployers pop
  g <- getStdGen
  let
    workers = take pop $ mkWorker <$> randoms g
    employments = genEmployments sd pop ((emps >>= paths) >>= (replicate 100))
    employers = emps >>= paths
    day = sd
    in return $ Sim day (zip workers employments) employers

printSim :: Sim -> [Row]
printSim sim = [ Row worker employment | (worker, employment) <- workers sim]

deptName :: Department -> Text
deptName d = foldr f "" d
  where
    f ent "" = (name ent)
    f ent acc = (name ent) <> " : " <> acc

instance ToField Day where
  toField day = fromString $ showJulian day :: ByteString

instance ToField Department where
  toField = fromString . T.unpack . deptName

data Row = Row Entity (Maybe Employment)

instance ToNamedRecord Row where
  toNamedRecord (Row (Entity uuid name lastName) emp) =
    namedRecord [ "name" .= name
                , "lastName" .= lastName
                , "uuid" .= (show uuid :: ByteString)
                ] <> toNamedRecord emp
data Config = Config { population :: Int
                     , startDate :: Day
                     }

loadConfig :: Int -> Day -> Config
loadConfig = Config

generateImport :: Config -> IO ()
generateImport config = do
  sim <- initSim config
  putStr . encodeByName (header ["name", "lastName", "uuid", "dept", "hired", "fired"]) $ printSim sim
