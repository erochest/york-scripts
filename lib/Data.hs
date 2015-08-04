{-# LANGUAGE RecordWildCards #-}


module Data where


import           Data.Foldable
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import qualified Data.List.NonEmpty  as NE
import           Data.Semigroup

import           Types


summarizeCall :: RollCall -> BillSummary
summarizeCall Call{..} =
    BillSum (congress bill) (number bill) (billInfo bill)
            (length rYes) (length rNos) (length dYes) (length dNos)
            (resultMetric result)
    where
        (rYes, dYes) = L.partition (==R) $ ayes votes
        (rNos, dNos) = L.partition (==R) $ nos  votes

indexByBill :: Foldable t => t RollCall -> RollCallIndex
indexByBill = foldl' step M.empty
    where
        step i rc = M.insertWith (++) (billKey $ bill rc) [rc] i

billKey :: Bill -> BillKey
billKey Bill{..} = (congress, number, billChamber billInfo)

getLastRollCall :: RollCallIndex -> RollCallLast
getLastRollCall = fmap (sconcat . fmap Max . NE.fromList) . M.filter (not . L.null)

