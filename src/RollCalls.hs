{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}


module Main where
-- module RollCalls where


import           Control.Arrow        ((&&&))
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types     (Parser)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as Csv
import           Data.Data
import           Data.Either
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict  as M
import qualified Data.List            as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Text            as T
import           Data.Time
import           GHC.Generics
import           Options.Applicative  hiding (Parser, info)
import qualified Options.Applicative  as O
import           System.Directory
import           System.FilePath

import           Debug.Trace


data Chamber
        = Senate
        | House
        deriving (Show, Eq, Typeable, Data, Generic)

instance FromJSON Chamber where
    parseJSON (String chamber)
        | T.null chamber        = mzero
        | T.head chamber == 'h' = pure House
        | T.head chamber == 's' = pure Senate
        | otherwise             = mzero

instance Csv.ToField Chamber where
    toField Senate = "s"
    toField House  = "h"

instance Hashable Chamber

data BillType
        = PlainBill
        | ConRes
        | JRes
        | Res
        deriving (Show, Eq, Typeable, Data, Generic)

instance FromJSON BillType where
    parseJSON (String "hr") = pure PlainBill
    parseJSON (String "s")  = pure PlainBill
    parseJSON (String billType)
        | T.null billType             = mzero
        | T.tail billType == "conres" = pure ConRes
        | T.tail billType == "jres"   = pure JRes
        | T.tail billType == "res"    = pure Res
        | otherwise                   = mzero

instance Csv.ToField BillType where
    toField PlainBill = ""
    toField ConRes    = "conres"
    toField JRes      = "jres"
    toField Res       = "res"

instance Hashable BillType

data BillInfo
        = BillInfo
        { billChamber  :: !Chamber
        , billInfoType :: !BillType
        } deriving (Show, Eq, Typeable, Data, Generic)

instance FromJSON BillInfo where
    parseJSON json = BillInfo <$> parseJSON json <*> parseJSON json

instance Csv.ToField BillInfo where
    toField (BillInfo Senate PlainBill) = "s"
    toField (BillInfo House  PlainBill) = "hr"
    toField (BillInfo c      t)         = Csv.toField c <> Csv.toField t

instance Hashable BillInfo

data Bill
        = Bill
        { congress :: !Int
        , number   :: !Int
        , billInfo :: !BillInfo
        } deriving (Show, Eq, Typeable, Data, Generic)

instance FromJSON Bill where
    parseJSON (Object o) =   Bill
                         <$> o .: "congress"
                         <*> o .: "number"
                         <*> o .: "type"
    parseJSON _          = mzero

instance Hashable Bill

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

parties :: Value -> Parser [Party]
parties = mapM parseJSON . (^.. _Array . traverse . key "party")

firstKey :: [T.Text] -> Object -> Parser Value
firstKey keys o = foldl' (step o) empty keys <|> pure (Array [])
    where
        step :: Object -> Parser Value -> T.Text -> Parser Value
        step o p k = p <|> o .: k

instance FromJSON VoteCall where
    parseJSON (Object o) =   Votes
                         <$> (firstKey ["Aye", "Yea"] o >>= parties)
                         <*> (firstKey ["Nay", "No" ] o >>= parties)
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

resultMetric :: BillResult -> Int
resultMetric AgreedTo                              = 1
resultMetric AmendmentAgreedTo                     = 1
resultMetric AmendmentGermane                      = 1
resultMetric AmendmentNotGermane                   = -1
resultMetric AmendmentRejected                     = -1
resultMetric BillDefeated                          = -1
resultMetric BillPassed                            = 1
resultMetric Boehner                               = 0
resultMetric ClotureMotionAgreedTo                 = 1
resultMetric ClotureMotionRejected                 = -1
resultMetric ClotureOnTheMotionToProceedAgreedTo   = 1
resultMetric ClotureOnTheMotionToProceedRejected   = -1
resultMetric ConcurrentResolutionAgreedTo          = 1
resultMetric ConcurrentResolutionRejected          = -1
resultMetric ConferenceReportAgreedTo              = 1
resultMetric DecisionOfChairNotSustained           = -1
resultMetric DecisionOfChairSustained              = 1
resultMetric Failed                                = -1
resultMetric Guilty                                = -1
resultMetric Hastert                               = 0
resultMetric JointResolutionDefeated               = -1
resultMetric JointResolutionPassed                 = 1
resultMetric MotionAgreedTo                        = 1
resultMetric MotionRejected                        = -1
resultMetric MotionForAttendanceAgreedTo           = 1
resultMetric MotionToAdjournAgreedTo               = 1
resultMetric MotionToAdjournRejected               = -1
resultMetric MotionToProceedAgreedTo               = 1
resultMetric MotionToProceedRejected               = -1
resultMetric MotionToRecommitRejected              = -1
resultMetric MotionToReconsiderAgreedTo            = 1
resultMetric MotionToReferRejected                 = -1
resultMetric MotionToTableAgreedTo                 = 1
resultMetric MotionToTableFailed                   = -1
resultMetric MotionToTablemOtionToRecommitAgreedTo = 1
resultMetric NominationConfirmed                   = 1
resultMetric ObjectionNotSustained                 = 1
resultMetric Passed                                = 1
resultMetric Pelosi                                = 0
resultMetric PointOfOrderNotSustained              = -1
resultMetric PointOfOrderNotWellTaken              = -1
resultMetric ResolutionAgreedTo                    = 1
resultMetric ResolutionRejected                    = -1
resultMetric ResolutionOfRatificationAgreedTo      = 1
resultMetric ResolutionOfRatificationRejected      = -1
resultMetric VetoOverridden                        = -1

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
        { bill     :: !Bill
        , callDate :: !ZonedTime
        , result   :: !BillResult
        , votes    :: !VoteCall
        } deriving (Show, Typeable, Data)

instance FromJSON RollCall where
    parseJSON (Object o) =   Call
                         <$> o .: "bill"
                         <*> o .: "date"
                         <*> o .: "result"
                         <*> o .: "votes"
    parseJSON _          =   mzero

type BillKey       = (Int, Int, Chamber)
type RollCallIndex = M.HashMap BillKey [RollCall]
type RollCallLast  = M.HashMap BillKey (Last RollCall)

data BillSummary
        = BillSum
        { sumCongress :: !Int
        , sumNumber   :: !Int
        , sumInfo     :: !BillInfo
        , sumRYes     :: !Int
        , sumRNo      :: !Int
        , sumDYes     :: !Int
        , sumDNo      :: !Int
        , sumResult   :: !Int
        } deriving (Show, Eq, Typeable, Data)

instance Csv.ToNamedRecord BillSummary where
    toNamedRecord BillSum{..} =
        Csv.namedRecord [ "congress" Csv..= sumCongress
                        , "chamber"  Csv..= billChamber sumInfo
                        , "type"     Csv..= sumInfo
                        , "bill#"    Csv..= sumNumber
                        , "R yeas"   Csv..= sumRYes
                        , "R nays"   Csv..= sumRNo
                        , "D yeas"   Csv..= sumDYes
                        , "D nays"   Csv..= sumDNo
                        , "result"   Csv..= sumResult
                        ]

instance Csv.DefaultOrdered BillSummary where
    headerOrder _ = Csv.header [ "congress"
                               , "chamber"
                               , "type"
                               , "bill#"
                               , "R yeas"
                               , "R nays"
                               , "D yeas"
                               , "D nays"
                               , "result"
                               ]


summarizeCall :: RollCall -> BillSummary
summarizeCall Call{..} =
    BillSum (congress bill) (number bill) (billInfo bill)
            (length rYes) (length rNos) (length dYes) (length dNos)
            (resultMetric result)
    where
        (rYes, dYes) = L.partition (==R) $ ayes votes
        (rNos, dNos) = L.partition (==R) $ nos  votes

decodeCall :: B.ByteString -> Maybe RollCall
decodeCall = decode . BL.fromStrict

decodeEitherCall :: B.ByteString -> Either String RollCall
decodeEitherCall = eitherDecode' . BL.fromStrict

info :: Show a => Bool -> String -> a -> a
info False _   x = x
info True  msg x = trace (msg ++ ": " ++ show x) x

readDecode :: Bool -> FilePath -> IO (Either String RollCall)
readDecode verbose filename =
    info verbose ("OUTPUT " ++ filename) . decodeEitherCall <$> B.readFile filename

indexByBill :: Foldable t => t RollCall -> RollCallIndex
indexByBill = foldl' step M.empty
    where
        step i rc = M.insertWith (++) (billKey $ bill rc) [rc] i

billKey :: Bill -> BillKey
billKey Bill{..} = (congress, number, billChamber billInfo)

getLastRollCall :: RollCallIndex -> RollCallLast
getLastRollCall = fmap (foldMap (Last . Just) . sortRollCalls)

sortRollCalls :: [RollCall] -> [RollCall]
sortRollCalls = L.sortBy (comparing (zonedTimeToUTC . callDate))

main :: IO ()
main = do
    Options{..} <- execParser opts
    BL.writeFile outputFile
        .   Csv.encodeDefaultOrderedByName
        .   map summarizeCall
        .   mapMaybe getLast
        .   M.elems
        .   getLastRollCall
        .   indexByBill
        .   rights
        =<< mapM (readDecode verbose)
        =<< walk inputDir


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

data Options
        = Options
        { inputDir   :: !FilePath
        , outputFile :: !FilePath
        , verbose    :: !Bool
        } deriving (Show, Eq)

opts' :: O.Parser Options
opts' =   Options
      <$> strArgument (metavar "INPUT_DIR"   <> help "The input directory.")
      <*> strArgument (metavar "OUTPUT_FILE" <> help "The output file.")
      <*> switch (  short 'v' <> long "verbose"
                 <> help "Output extra debugging information.")

opts :: ParserInfo Options
opts = O.info (helper <*> opts')
        (  fullDesc
        <> progDesc "Process the roll call data."
        <> header "roll-calls -- process the roll call data"
        )
