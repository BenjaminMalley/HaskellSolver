data Horn = Horn { head :: String, body :: Maybe [String] } deriving (Read, Eq, Show)
newtype KB = [Horn]


