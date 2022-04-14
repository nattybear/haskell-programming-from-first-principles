data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica  = True
isSouthAfrica Galapagos    = False
isSouthAfrica Antarctica   = False
isSouthAfrica Australia    = False
isSouthAfrica SouthAmerica = False
