{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Conduit
import           Control.Exception
import           Data.Csv
import           Data.Csv.Conduit
import qualified Data.Text         as T
import           Data.Text.Format
import           Data.Text.Lazy    (toStrict)

import           Opts


main :: IO ()
main = parseOpts >>= yorkScripts

yorkScripts :: Options -> IO ()
yorkScripts BillId =
    runResourceT $  stdinC
                 $= fromCsvLiftError liftError defaultDecodeOptions NoHeader
                 $= mapC line
                 $$ toCsv defaultEncodeOptions
                 =$ stdoutC

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
line ("KeyID":"BillID":header) = "KeyID" : "BillID" : "BillID2" : header
line (keyId:billId:row)        = keyId : billId : field billId : row
line row                       = row

field :: T.Text -> T.Text
field = toStrict . format "{}{} ({})" . parseBillId . T.split (== '-')

parseBillId :: [T.Text] -> (T.Text, T.Text, T.Text)
parseBillId (session:house:bill:_) = (T.take 1 house, bill, session)
parseBillId _ = ("ERROR", "", "")

liftError :: CsvParseError -> IOException
liftError (CsvParseError _ msg)  = undefined
liftError (IncrementalError msg) = undefined
