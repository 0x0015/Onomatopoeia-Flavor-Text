{-# LANGUAGE DeriveGeneric, DataKinds, DerivingVia, OverloadedStrings #-}

module Main where
import Data.Text as T (Text, lines, pack, strip)
import Data.Text.IO (readFile)
import Data.Text.Internal.Search (indices)
import Data.ByteString.Lazy (readFile)
import Data.Aeson
import Data.Maybe (isNothing, fromMaybe, isJust)
import Data.Coerce (coerce)
import GHC.Generics (Generic)

data Card = Card{
    object :: Text,
    name :: Text,
    oracle_text :: Maybe Text,
    flavor_text :: Maybe Text
} deriving (Generic, Show)

newtype CardList = CardList [Card] deriving (Show, Generic)

instance FromJSON Card
instance FromJSON CardList

findOnomatopoeicFlavorTexts :: CardList -> [Text] -> [(Text, [Text])]
findOnomatopoeicFlavorTexts cards onomatopoeias = do
    let cardsWithFlavorText = filter (isJust . flavor_text) (coerce cards :: [Card])
    let foundOnomatopoeiasInText text = filter (/= "") [if (not . null) $ indices onomatopoeia text then onomatopoeia else "" | onomatopoeia <- onomatopoeias]
    let searchedFlavorTexts = [(name card, map strip $ foundOnomatopoeiasInText (fromMaybe "" $ flavor_text card)) | card <- cardsWithFlavorText]
    let onlyPositiveResults = filter (not . null . snd) searchedFlavorTexts
    onlyPositiveResults


main :: IO ()
main = do
    onomatopoeias <- Data.Text.IO.readFile "onomatopoeias.txt"
    let onomatopoeiaWords = [T.pack " " <> onomatopoeia <> T.pack " " | onomatopoeia <- T.lines onomatopoeias]
    jsonText <- Data.ByteString.Lazy.readFile "default-cards.json"
    case eitherDecode jsonText :: Either String CardList of
        Left err -> error err
        Right val -> print $ findOnomatopoeicFlavorTexts val onomatopoeiaWords


