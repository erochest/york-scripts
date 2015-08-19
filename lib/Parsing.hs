module Parsing where


import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Either
import qualified Data.List            as L

import           Types
import           Utils


decodeCall :: B.ByteString -> Maybe RollCall
decodeCall = decode . BL.fromStrict

decodeEitherCall :: B.ByteString -> Either String RollCall
decodeEitherCall = eitherDecode' . BL.fromStrict

readDecode :: Bool -> FilePath -> IO (Either String RollCall)
readDecode verbose filename =
    info verbose ("INPUT " ++ filename) . decodeEitherCall <$> B.readFile filename

readDecodeDir :: Bool -> FilePath -> IO [RollCall]
readDecodeDir verbose =   fmap rights . mapM (readDecode verbose) . filter (".json" `L.isSuffixOf`)
                      <=< walk
