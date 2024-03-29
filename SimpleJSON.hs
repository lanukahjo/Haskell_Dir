module ExportEverything 
where

data JValue = JString String | JNumber Double | JBool Bool | JNull | JObject [(String,JValue)] | JArray [JValue]
              deriving(Show,Eq,Ord)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool k) = Just k
getBool _         = Nothing

getObject :: JValue -> Maybe [(String,JValue)]
getObject (JObject o) = Just o
getObject _           = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray o) = Just o
getArray _          = Nothing

isNull :: JValue -> Bool
isNull v = v==JNull