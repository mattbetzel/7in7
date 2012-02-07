module SimpleJSON (JValue(..), toString) where
import Data.List (intersperse)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

toString :: JValue -> String
toString (JString s) = show s
toString (JNumber n) = show n
toString (JBool True)   = "true"
toString (JBool False)   = "false"
toString (JNull)     = "null"
toString (JObject o) = (objectEnclose . joinWithComma . map mapper) o
    where mapper (key, value) = show key ++ ":" ++ toString value
toString (JArray a)  = (arrayEnclose . joinWithComma . map (toString)) a

joinWithComma = concat . (intersperse ",")

enclose a b xs = a : xs ++ [b]
arrayEnclose = enclose '[' ']'
objectEnclose = enclose '{' '}'
