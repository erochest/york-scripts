

module Opts
    ( parseOpts
    , Options(..)
    ) where


import           Options.Applicative


data Options
        = BillId
        {
        } deriving (Show, Eq)

billIdParser :: Parser Options
billIdParser = pure BillId

opts' :: Parser Options
opts' =
    subparser
        (  command "bill-id" (info (helper <*> billIdParser)
                                   (  fullDesc
                                   <> progDesc "This operates on STDIN/STDOUT."
                                   <> header "york-scripts bill-id -- generate full bill id"))
        )

opts :: ParserInfo Options
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Process data files for John York."
            <> header "york-scripts -- process data files.")

parseOpts :: IO Options
parseOpts = execParser opts
