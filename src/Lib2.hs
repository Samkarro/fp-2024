{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

data Query
  = Single [String] String          -- Single artist - track
  | EP [String] String [Query]       -- EP artist - ep-name and a bunch of singles
  | Album [String] String [Query]    -- Album artist - album-name and a bunch of singles
  | Artist [String]                 -- List of artist aliases
  | TrackCollection [String]        -- List of track names


instance Eq Query where
  (Single l1 s1)==(Single l2 s2) = s1 == s2 && l1 == l2
  (EP l1 s1 l11)==(EP l2 s2 l22) = l1 == l2 && s1 == s2 && l11 == l22
  (Album l1 s1 l11)==(Album l2 s2 l22) = l1 == l2 && s1 == s2 && l11 == l22
  (Artist l1)==(Artist l2) = l1 == l2
  (TrackCollection l1)==(TrackCollection l2) = l1 == l2
  _ == _ = False

instance Show Query where
  show (Single l1 s1) = removeChars(" " ++ show l1 ++ " - " ++ show s1 ++ " ")
  show (EP l1 s1 l11) = removeChars("EP: " ++ show l1 ++ " - " ++ show s1 ++ ". Tracks: " ++ show l11) 
  show (Album l1 s1 l11) = "Album: " ++ show l1 ++ " - " ++ show s1 ++ ". | " ++ "Tracks: " ++ show l11
  show (Artist l1) = removeChars(show l1)
  show (TrackCollection l1) = removeChars("Track(s): " ++ show l1)
  
parseQuery :: String -> Either String Query
parseQuery input
  | null input = Left "Input cannot be empty"
  | length input < 2 = Left "Input must be at least 2 characters long"
  | otherwise =
      let tokens = words input
      in parseTokens tokens

removeChars :: String -> String
removeChars input = [c | c <- input, c `notElem` "\\[]()\""]

parseTokens :: [String] -> Either String Query
parseTokens tokens = 
  or2
    (trySingle tokens)
    (tryEP tokens)
    `or2` (tryAlbum tokens)
    `or2` (tryArtists tokens)
    `or2` (tryTrackCollection tokens)

trySingle :: [String] -> Either String Query
trySingle [artist, "-", track] = Right (Single [artist] track)
trySingle _ = Left "Invalid single query format"

tryEP :: [String] -> Either String Query
tryEP (artist:"-":epName:"|":trackTokens) = do
    tracks <- parseTrackCollection trackTokens
    Right (EP [artist] epName tracks)
tryEP _ = Left "Invalid EP query format"

tryAlbum :: [String] -> Either String Query
tryAlbum (artist:"-":albumName:"|":trackTokens) = do
    tracks <- parseTrackCollection trackTokens
    Right (Album [artist] albumName tracks)
tryAlbum _ = Left "Invalid album query format"

tryArtists :: [String] -> Either String Query
tryArtists artists | length artists > 1 = Right (Artist artists)
tryArtists _ = Left "Invalid artist query format"

tryTrackCollection :: [String] -> Either String Query
tryTrackCollection trackNames = Right (TrackCollection trackNames)

parseTrackCollection :: [String] -> Either String [Query]
parseTrackCollection [] = Right []
parseTrackCollection trackNames = Right [TrackCollection trackNames]

or2 :: Either String a -> Either String a -> Either String a
or2 (Right x) _ = Right x
or2 (Left _) y = y

and2 :: (a -> Either String b) -> (b -> Either String c) -> a -> Either String c
and2 p1 p2 x = case p1 x of
    Right y -> p2 y
    Left err -> Left err

data State = State
  { queries :: [Query] 
  } deriving (Show, Eq)

emptyState :: State
emptyState = State { queries = [] }

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = 
  let newState = st { queries = query : queries st }
  in Right (Just ("Processed query: " ++ show query), newState)
