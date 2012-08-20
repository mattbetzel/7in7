import qualified Data.Map as Map
import Data.Char
import Data.List
import Control.Applicative

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

orError :: Maybe a -> b -> Either b a
Just v `orError` _ = Right v
_      `orError` e = Left e

isTrue :: a -> Bool -> Maybe a
isTrue a p = if p then Just a else Nothing

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup locker lockers = do
        (state, code) <- Map.lookup locker lockers `orError` "Locker not found"
        isTrue code (state == Free) `orError` "Locker is taken"

lockers :: LockerMap
lockers = Map.fromList [ (1, (Free, "123")), (2, (Taken, "456")) ]
