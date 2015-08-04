{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as Csv
import qualified Data.HashMap.Strict  as M
import           Data.Semigroup
import           Options.Applicative

import           Data
import           Parsing


main :: IO ()
main = do
    Options{..} <- execParser opts
    BL.writeFile outputFile
        .   Csv.encodeDefaultOrderedByName
        .   map (summarizeCall . getMax)
        .   M.elems
        .   getLastRollCall
        .   indexByBill
        =<< readDecodeDir verbose inputDir


data Options
        = Options
        { inputDir   :: !FilePath
        , outputFile :: !FilePath
        , verbose    :: !Bool
        } deriving (Show, Eq)

opts' :: Parser Options
opts' =   Options
      <$> strArgument (metavar "INPUT_DIR"   `mappend` help "The input directory.")
      <*> strArgument (metavar "OUTPUT_FILE" `mappend` help "The output file.")
      <*> switch (  short 'v' `mappend` long "verbose"
                 `mappend` help "Output extra debugging information.")

opts :: ParserInfo Options
opts = info (helper <*> opts')
        (  fullDesc
        `mappend` progDesc "Process the roll call data."
        `mappend` header "roll-calls -- process the roll call data"
        )
