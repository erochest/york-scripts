{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}


module Data where


import           Data.Foldable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.List           as L
import qualified Data.List.NonEmpty  as NE
import           Data.Semigroup

import           Types


keepCategories :: S.HashSet Category
keepCategories = [ PassageSuspension
                 , Passage
                 , Cloture
                 , VetoOverride
                 -- , Procedural
                 ]

filterCategories :: [RollCall] -> [RollCall]
filterCategories = filter ((`S.member` keepCategories) . category)

summarizeCall :: RollCall -> BillSummary
summarizeCall Call{..} =
    BillSum (congress bill) callDate voteChamber (number bill) category
            (billInfo bill) (length rYes) (length rNos)
            (length dYes) (length dNos)
            (resultMetric result)
    where
        (rYes, dYes) = L.partition (==R) $ ayes votes
        (rNos, dNos) = L.partition (==R) $ nos  votes

indexByBill :: Foldable t => t RollCall -> RollCallIndex
indexByBill = foldl' step M.empty
    where
        step i rc = M.insertWith (++) (rollCallKey rc) [rc] i

rollCallKey :: RollCall -> BillKey
rollCallKey Call{..} = (congress bill, number bill, voteChamber)

getLastRollCall :: RollCallIndex -> RollCallLast
getLastRollCall = fmap (sconcat . fmap Max . NE.fromList) . M.filter (not . L.null)
