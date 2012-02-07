import Data.Time (UTCTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (insert, nub)

data Range a = Range { start :: a, end :: a } deriving (Show)

data RangeContainer a b = RangeContainer { name :: b, range :: Range a } deriving (Show)

extractRange :: Ord a => RangeContainer a b ->  [a] -> [a]
extractRange (RangeContainer _ (Range start end)) xs = insert start (insert end xs)

groupIntoRanges :: [a] -> [Range a] 
groupIntoRanges (a:b:xs) = Range a b : groupIntoRanges (b : xs)
groupIntoRanges _ = []

determineRangeChanges :: (Eq a, Ord a) => Map String (RangeContainer a b) -> [Range a]
determineRangeChanges xs = groupIntoRanges (nub (Map.fold extractRange [] xs))

type DateRange = Range UTCTime
type EmploymentPeriod = RangeContainer UTCTime String

test1 = Map.fromList [("Jo", RangeContainer "Acme" (Range 1 5)), ("Ho", RangeContainer "OP" (Range 2 3)), ("Ha", RangeContainer "OP" (Range 3 5))]
