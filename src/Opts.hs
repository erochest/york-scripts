{-# LANGUAGE OverloadedStrings #-}


module Opts
    ( parseOpts
    , Options(..)
    ) where


import           Options.Applicative


data Options
        = BillId
        | RollCall { inputDir   :: !FilePath
                   , outputFile :: !FilePath
                   , verbose    :: !Bool
                   }
        | CallData { inputDir   :: !FilePath
                   }
        deriving (Show, Eq)

billIdParser :: Parser Options
billIdParser = pure BillId

rollCallParser :: Parser Options
rollCallParser =
        RollCall
    <$> strArgument (metavar "INPUT_DIR"   <> help "The input directory.")
    <*> strArgument (metavar "OUTPUT_FILE" <> help "The output file.")
    <*> switch (  short 'v' <> long "verbose"
               <> help "Output extra debugging information.")

callDataParser :: Parser Options
callDataParser =
    CallData <$> strArgument (metavar "INPUT_DIR" <> help "The input directory.")

opts' :: Parser Options
opts' =
    subparser
        (  command "bill-id" (info (helper <*> billIdParser)
                                   (  fullDesc
                                   <> progDesc "This operates on STDIN/STDOUT."
                                   <> header "york-scripts bill-id -- generate full bill id"))
        <> command "roll-call" (info (helper <*> rollCallParser)
                                      (  fullDesc
                                      <> progDesc "Process the roll call data."
                                      <> header "york-scripts roll-calls"))
        <> command "call-data" (info (helper <*> callDataParser)
                                     (  fullDesc
                                     <> progDesc "Summarize a single field from the roll call data."
                                     <> header "york-scripts call-data"))
        )

opts :: ParserInfo Options
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Process data files for John York."
            <> header "york-scripts -- process data files.")

parseOpts :: IO Options
parseOpts = execParser opts
