{-# LANGUAGE OverloadedStrings #-}

import Data.List (transpose)
import Database.SQLite.Simple
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, fromMaybe, isJust)

type Mood = String

type Tense = String

type Prop = NaturalWord -> String

data NaturalWord = NaturalWord
  { infinitive :: String,
    mood :: Mood,
    tense :: Tense,
    translation :: String,
    fps :: String,
    sps :: String,
    tps :: String,
    fpp :: String,
    spp :: String,
    tpp :: String
  }
  deriving (Show)

instance FromRow NaturalWord where
  fromRow = NaturalWord <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data Grid = Grid Int Int deriving (Show)

moods :: [Mood]
moods = ["Indicativo", "Subjuntivo", "Imperativo Afirmativo", "Imperativo Negativo"]

persons :: [(String, Prop)]
persons = [("Yo", fps), ("Tú", sps), ("él/ella/Ud.", tps), ("nosotros", fpp), ("vosotros", spp), ("ellos/ellas/Uds.", tpp)]

tenses :: [Tense]
tenses = ["Presente", "Pretérito", "Imperfecto", "Imperfecto", "Futuro"]

padR :: Int -> String -> String
padR n s
  | length s < n = s ++ replicate (n - length s) ' '
  | otherwise = s
 
formatLine :: [(Int, String)] -> String
formatLine ws = foldl1 (++) $ map (\(length', line) -> padR (max (length'+1) 12) line) ws

filterTense :: [NaturalWord] -> Tense -> Maybe NaturalWord
filterTense nws t = listToMaybe $ filter ((== t) . tense) nws
 
applyProp :: (a -> String) -> Maybe a -> Maybe String
applyProp f m = if x == "" then Nothing else Just x 
  where x = fromMaybe "" (f <$> m)
 
filterAllBlank :: [(String, [Maybe String])] -> [(String, [Maybe String])]
filterAllBlank = filter ((any isJust) . snd) 

mergeTuple :: (a, [Maybe a]) -> [Maybe a]
mergeTuple (label, values') = Just label:values'

printMood :: [NaturalWord] -> Mood -> IO ()
printMood nws m = do
  putStrLn $ "\n" ++ m ++ "\n"
  mapM_ printRow filteredByColumn
  where
    moodMatches = filter ((== m) . mood) nws

    tenseValues :: [Maybe NaturalWord]
    tenseValues = map (filterTense moodMatches) tenses

    personLabels :: [String]
    personLabels = map fst persons

    tensesForPerson :: (Maybe NaturalWord -> Maybe String) -> [Maybe String]
    tensesForPerson = flip map tenseValues

    values :: [[Maybe String]]
    values = map (tensesForPerson . applyProp . snd) persons

    filteredByRow :: [(String, [Maybe String])]
    filteredByRow = filterAllBlank $ zip personLabels values
  
    withPersonLabel :: [[Maybe String]]
    withPersonLabel = (map mergeTuple filteredByRow)
 
    filteredByColumn :: [[String]]
    filteredByColumn = transpose $ map ((map (fromMaybe "")) . mergeTuple) $ filterAllBlank $ zip ("":tenses) $ transpose withPersonLabel

    lengths :: [Int]
    lengths = map ((foldr1 max) . (map length)) $ (transpose filteredByColumn)

    printRow row = putStrLn $ formatLine (zip lengths row) 

main :: IO ()
main = do
  verb : _ <- getArgs
  conn <- open "conjugation.db"

  rows <- query conn "SELECT * from verbs where infinitive=?" (Only (verb :: String)) :: IO [NaturalWord]
  mapM_ (printMood rows) moods

  close conn
