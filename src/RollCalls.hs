{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}


module Main where


import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types     (Parser)
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
        = D | R | I
        deriving (Show, Eq, Typeable, Data)

instance FromJSON Party where
    parseJSON (String "D") = pure D
    parseJSON (String "R") = pure R
    parseJSON (String "I") = pure I
    parseJSON _            = mzero

data VoteCall
        = Votes
        { ayes :: ![Party]
        , nos  :: ![Party]
        } deriving (Show, Eq, Typeable, Data)

instance FromJSON VoteCall where
    parseJSON (Object o) =   Votes
                         <$> (o .: "Aye" >>= parties)
                         <*> (o .: "No"  >>= parties)
                         where
                             parties :: Value -> Parser [Party]
                             parties = mapM parseJSON
                                     . (^.. _Array . traverse . key "party")
    parseJSON _          =   mzero

data BillResult
        = AgreedTo
        | AmendmentAgreedTo
        | AmendmentGermane
        | AmendmentNotGermane
        | AmendmentRejected
        | BillDefeated
        | BillPassed
        | Boehner
        | ClotureMotionAgreedTo
        | ClotureMotionRejected
        | ClotureOnTheMotionToProceedAgreedTo
        | ClotureOnTheMotionToProceedRejected
        | ConcurrentResolutionAgreedTo
        | ConcurrentResolutionRejected
        | ConferenceReportAgreedTo
        | DecisionOfChairNotSustained
        | DecisionOfChairSustained
        | Failed
        | Guilty
        | Hastert
        | JointResolutionDefeated
        | JointResolutionPassed
        | MotionAgreedTo
        | MotionRejected
        | MotionForAttendanceAgreedTo
        | MotionToAdjournAgreedTo
        | MotionToAdjournRejected
        | MotionToProceedAgreedTo
        | MotionToProceedRejected
        | MotionToRecommitRejected
        | MotionToReconsiderAgreedTo
        | MotionToReferRejected
        | MotionToTableAgreedTo
        | MotionToTableFailed
        | MotionToTablemOtionToRecommitAgreedTo
        | NominationConfirmed
        | ObjectionNotSustained
        | Passed
        | Pelosi
        | PointOfOrderNotSustained
        | PointOfOrderNotWellTaken
        | ResolutionAgreedTo
        | ResolutionRejected
        | ResolutionOfRatificationAgreedTo
        | ResolutionOfRatificationRejected
        | VetoOverridden
        deriving (Show, Eq, Typeable, Data)

instance FromJSON BillResult where
    parseJSON (String "Agreed to") = pure AgreedTo
    parseJSON (String "Amendment Agreed to") = pure AmendmentAgreedTo
    parseJSON (String "Amendment Germane") = pure AmendmentGermane
    parseJSON (String "Amendment Not Germane") = pure AmendmentNotGermane
    parseJSON (String "Amendment Rejected") = pure AmendmentRejected
    parseJSON (String "Bill Defeated") = pure BillDefeated
    parseJSON (String "Bill Passed") = pure BillPassed
    parseJSON (String "Boehner") = pure Boehner
    parseJSON (String "Cloture Motion Agreed to") = pure ClotureMotionAgreedTo
    parseJSON (String "Cloture Motion Rejected") = pure ClotureMotionRejected
    parseJSON (String "Cloture on the Motion to Proceed Agreed to") =
        pure ClotureOnTheMotionToProceedAgreedTo
    parseJSON (String "Cloture on the Motion to Proceed Rejected") =
        pure ClotureOnTheMotionToProceedRejected
    parseJSON (String "Concurrent Resolution Agreed to") =
        pure ConcurrentResolutionAgreedTo
    parseJSON (String "Concurrent Resolution Rejected") =
        pure ConcurrentResolutionRejected
    parseJSON (String "Conference Report Agreed to") = pure ConferenceReportAgreedTo
    parseJSON (String "Decision of Chair Not Sustained") =
        pure DecisionOfChairNotSustained
    parseJSON (String "Decision of Chair Sustained") = pure DecisionOfChairSustained
    parseJSON (String "Failed") = pure Failed
    parseJSON (String "Guilty") = pure Guilty
    parseJSON (String "Hastert") = pure Hastert
    parseJSON (String "Joint Resolution Defeated") = pure JointResolutionDefeated
    parseJSON (String "Joint Resolution Passed") = pure JointResolutionPassed
    parseJSON (String "Motion Agreed to") = pure MotionAgreedTo
    parseJSON (String "Motion Rejected") = pure MotionRejected
    parseJSON (String "Motion for Attendance Agreed to") =
        pure MotionForAttendanceAgreedTo
    parseJSON (String "Motion to Adjourn Agreed to") = pure MotionToAdjournAgreedTo
    parseJSON (String "Motion to Adjourn Rejected") = pure MotionToAdjournRejected
    parseJSON (String "Motion to Proceed Agreed to") = pure MotionToProceedAgreedTo
    parseJSON (String "Motion to Proceed Rejected") = pure MotionToProceedRejected
    parseJSON (String "Motion to Recommit Rejected") = pure MotionToRecommitRejected
    parseJSON (String "Motion to Reconsider Agreed to") =
        pure MotionToReconsiderAgreedTo
    parseJSON (String "Motion to Refer Rejected") = pure MotionToReferRejected
    parseJSON (String "Motion to Table Agreed to") = pure MotionToTableAgreedTo
    parseJSON (String "Motion to Table Failed") = pure MotionToTableFailed
    parseJSON (String "Motion to Table Motion to Recommit Agreed to") =
        pure MotionToTablemOtionToRecommitAgreedTo
    parseJSON (String "Nomination Confirmed") = pure NominationConfirmed
    parseJSON (String "Objection Not Sustained") = pure ObjectionNotSustained
    parseJSON (String "Passed") = pure Passed
    parseJSON (String "Pelosi") = pure Pelosi
    parseJSON (String "Point of Order Not Sustained") = pure PointOfOrderNotSustained
    parseJSON (String "Point of Order Not Well Taken") =
        pure PointOfOrderNotWellTaken
    parseJSON (String "Resolution Agreed to") = pure ResolutionAgreedTo
    parseJSON (String "Resolution Rejected") = pure ResolutionRejected
    parseJSON (String "Resolution of Ratification Agreed to") =
        pure ResolutionOfRatificationAgreedTo
    parseJSON (String "Resolution of Ratification Rejected") =
        pure ResolutionOfRatificationRejected
    parseJSON (String "Veto Overridden") = pure VetoOverridden
    parseJSON _ = mzero

data RollCall
        = Call
        { bill   :: !Bill
        , result :: !BillResult
        , votes  :: !VoteCall
        } deriving (Show, Eq, Typeable, Data)

instance FromJSON RollCall where
    parseJSON (Object o) =   Call
                         <$> o .: "bill"
                         <*> o .: "result"
                         <*> o .: "votes"
    parseJSON _          =   mzero

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

summarizeCall :: RollCall -> BillSummary
summarizeCall = undefined


main :: IO ()
main = do
    [dirname] <- getArgs
    files <- walk dirname
    mapM_ TIO.putStrLn . L.sort . S.toList =<< foldlM step S.empty files
    where
        step :: S.HashSet T.Text -> FilePath -> IO (S.HashSet T.Text)
        step s filename =
            maybe s (foldl' (flip S.insert) s)
                .   fmap (preview ( key "result"
                                  . _String
                                  ))
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
