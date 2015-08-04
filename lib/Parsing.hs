module Parsing where


import           Data.Aeson
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

import           Types
import           Utils


decodeCall :: B.ByteString -> Maybe RollCall
decodeCall = decode . BL.fromStrict

decodeEitherCall :: B.ByteString -> Either String RollCall
decodeEitherCall = eitherDecode' . BL.fromStrict

readDecode :: Bool -> FilePath -> IO (Either String RollCall)
readDecode verbose filename =
    info verbose ("OUTPUT " ++ filename) . decodeEitherCall <$> B.readFile filename

