{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict  as M
import qualified Data.List            as L
import           Data.Maybe
import           Data.Ord
import qualified Data.Text            as T
import qualified Data.Text.Format     as F
import           Options.Applicative

import           Utils                hiding (info)


category :: Value -> Maybe T.Text
category = preview (key "category" . _String)

frequencies :: (Hashable k, Eq k, Traversable t) => t k -> M.HashMap k Int
frequencies = foldl' step M.empty
    where
        step m k = M.insertWith (+) k 1 m


main :: IO ()
main = do
    Options{..} <- execParser opts
    mapM_ (F.print "{} {}\n" . bimap (F.left 20 ' ') (F.right 5 ' '))
        .   L.sortBy (comparing (Down . snd))
        .   M.toList
        .   frequencies
        .   mapMaybe (join . fmap category . decode . BL.fromStrict)
        =<< mapM B.readFile
        =<< walk inputDir


data Options
        = Options
        { inputDir :: !FilePath
        } deriving (Show, Eq)

opts' :: Parser Options
opts' =   Options
      <$> strArgument (metavar "INPUT_DIR"   `mappend` help "The input directory.")

opts :: ParserInfo Options
opts = info (helper <*> opts')
        (  fullDesc
        `mappend` progDesc "Summarize a single field from the roll call data."
        `mappend` header "call-data -- summarize a single field from the roll call data"
        )
