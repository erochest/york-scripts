{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Conduit
import           Data.Hashable
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson           as A
import           Data.Aeson.Lens
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import qualified Data.Csv             as Csv
import           Data.Csv.Conduit
import           Data.Foldable
import qualified Data.HashMap.Strict  as M
import qualified Data.List            as L
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import qualified Data.Text            as T
import           Data.Text.Format
import qualified Data.Text.Format     as F
import           Data.Text.Lazy       (toStrict)

import           Data
import           Opts
import           Parsing
import           Utils                hiding (info)


main :: IO ()
main = parseOpts >>= yorkScripts

yorkScripts :: Options -> IO ()
yorkScripts BillId =
    runResourceT $  stdinC
                 $= fromCsvLiftError liftError defaultDecodeOptions NoHeader
                 $= mapC line
                 $$ toCsv defaultEncodeOptions
                 =$ stdoutC

yorkScripts RollCall{..} =
    BL.writeFile outputFile
        .   Csv.encodeDefaultOrderedByName
        .   map (summarizeCall . getMax)
        .   M.elems
        .   getLastRollCall
        .   indexByBill
        .   filterCategories
        =<< readDecodeDir verbose inputDir

yorkScripts CallData{..} =
    mapM_ (F.print "{} {}\n" . bimap (F.left 20 ' ') (F.right 5 ' '))
        .   L.sortBy (comparing (Down . snd))
        .   M.toList
        .   frequencies
        .   mapMaybe (join . fmap category . A.decode . BL.fromStrict)
        =<< mapM B.readFile
        =<< walk inputDir

-- key id
-- bill id
-- >>> bill id 2 <<<
-- cong
-- bill type
-- bill no
-- name full
-- description
-- intr date
-- intr month
-- year
-- major topic
-- sub topic code
-- cap major topic
-- cap sub topic
-- plaw
-- plaw date
-- plaw no
-- pass h
-- pass s
-- majority
-- party
-- chamber
-- commem
-- congress

line :: [T.Text] -> [T.Text]
line ("KeyID":"BillID":hdr) = "KeyID" : "BillID" : "BillID2" : hdr
line (keyId:billId:row)     = keyId : billId : field billId : row
line row                    = row

field :: T.Text -> T.Text
field = toStrict . format "{}{} ({})" . parseBillId . T.split (== '-')

parseBillId :: [T.Text] -> (T.Text, T.Text, T.Text)
parseBillId (session:house:bill:_) = (T.take 1 house, bill, session)
parseBillId _ = ("ERROR", "", "")

liftError :: CsvParseError -> IOException
liftError (CsvParseError _ _)  = undefined
liftError (IncrementalError _) = undefined

category :: Value -> Maybe T.Text
category = preview (key "category" . _String)

frequencies :: (Hashable k, Eq k, Traversable t) => t k -> M.HashMap k Int
frequencies = foldl' step M.empty
    where
        step m k = M.insertWith (+) k 1 m
