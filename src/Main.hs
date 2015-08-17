{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Conduit
import           Control.Exception
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import qualified Data.Csv             as Csv
import           Data.Csv.Conduit
import qualified Data.HashMap.Strict  as M
import           Data.Semigroup
import qualified Data.Text            as T
import           Data.Text.Format
import           Data.Text.Lazy       (toStrict)

import           Data
import           Opts
import           Parsing


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
