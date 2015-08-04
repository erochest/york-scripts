{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where
-- module RollCalls where


import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as Csv
import           Data.Either
import qualified Data.HashMap.Strict  as M
import qualified Data.List            as L
import           Data.Semigroup
import           Options.Applicative  hiding (Parser, info)
import qualified Options.Applicative  as O

import           Data
import           Parsing
import           Utils


main :: IO ()
main = do
    Options{..} <- execParser opts
    BL.writeFile outputFile
        .   Csv.encodeDefaultOrderedByName
        .   map (summarizeCall . getMax)
        .   M.elems
        .   getLastRollCall
        .   indexByBill
        .   rights
        =<< mapM (readDecode verbose) . filter (".json" `L.isSuffixOf`)
        =<< walk inputDir


data Options
        = Options
        { inputDir   :: !FilePath
        , outputFile :: !FilePath
        , verbose    :: !Bool
        } deriving (Show, Eq)

opts' :: O.Parser Options
opts' =   Options
      <$> strArgument (metavar "INPUT_DIR"   `mappend` help "The input directory.")
      <*> strArgument (metavar "OUTPUT_FILE" `mappend` help "The output file.")
      <*> switch (  short 'v' `mappend` long "verbose"
                 `mappend` help "Output extra debugging information.")

opts :: ParserInfo Options
opts = O.info (helper <*> opts')
        (  fullDesc
        `mappend` progDesc "Process the roll call data."
        `mappend` header "roll-calls -- process the roll call data"
        )
