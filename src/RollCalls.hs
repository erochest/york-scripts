{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}


module Main where


import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Data
import           Data.Foldable
import qualified Data.HashSet         as S
import qualified Data.List            as L
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Typeable
import           GHC.Generics
import           System.Directory
import           System.Environment
import           System.FilePath


data BillType
        = HConRes
        | HJRes
        | HR
        | HRes
        | S
        | SConRes
        | SJRes
        | SRes
        deriving (Show, Eq, Typeable, Data)

instance FromJSON BillType where
    parseJSON (String "hconres") = pure HConRes
    parseJSON (String "hjres")   = pure HJRes
    parseJSON (String "hr")      = pure HR
    parseJSON (String "hres")    = pure HRes
    parseJSON (String "s")       = pure S
    parseJSON (String "sconres") = pure SConRes
    parseJSON (String "sjres")   = pure SJRes
    parseJSON (String "sres")    = pure SRes
    parseJSON _                  = mzero

data Bill
        = Bill
        { congress :: !Int
        , number   :: !Int
        , billType :: !BillType
        } deriving (Show, Eq, Typeable, Data)

instance FromJSON Bill where
    parseJSON (Object o) =   Bill
                         <$> o .: "congress"
                         <*> o .: "number"
                         <*> o .: "type"
    parseJSON _          = mzero

data Party
        = D | R
        deriving (Show, Eq, Typeable, Data)

data VoteCall
        = Votes
        { ayes :: ![Party]
        , nos  :: ![Party]
        } deriving (Show, Eq, Typeable, Data)

data BillResult
        = Failed
        | Passed
        deriving (Show, Eq, Typeable, Data)

data RollCall
        = Call
        { bill   :: !Bill
        , result :: !BillResult
        , votes  :: !VoteCall
        } deriving (Show, Eq, Typeable, Data)

data BillSummary
        = BillSum
        { sumCongress :: !Int
        , sumNumber   :: !Int
        , sumType     :: !BillType
        , sumRYes     :: !Int
        , sumRNo      :: !Int
        , sumDYes     :: !Int
        , sumDNo      :: !Int
        , sumResult   :: !Int
        } deriving (Show, Eq, Typeable, Data)


main :: IO ()
main = do
    [dirname] <- getArgs
    files <- walk dirname
    mapM_ TIO.putStrLn . L.sort . S.toList =<< foldlM step S.empty files
    where
        step :: S.HashSet T.Text -> FilePath -> IO (S.HashSet T.Text)
        step s filename =
            maybe s (`S.insert` s)
                .   join
                .   fmap (preview (key "bill" . key "type" . _String))
                .   (decode :: BL.ByteString -> Maybe Value)
                .   BL.fromStrict
                <$> B.readFile filename


walk :: FilePath -> IO [FilePath]
walk dirname = do
    (dirs, files) <-  partitionM doesDirectoryExist
                  .   map (dirname </>)
                  .   filter (not . hidden)
                  =<< getDirectoryContents dirname
    concat . (files :) <$> mapM walk dirs
    where
        hidden []      = True
        hidden ('.':_) = True
        hidden _       = False


partitionM :: Monad m => (x -> m Bool) -> [x] -> m ([x], [x])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = do
    result   <- f x
    (ts, fs) <- partitionM f xs
    return $ if result
                 then (x:ts, fs)
                 else (ts, x:fs)
