{-# LANGUAGE OverloadedStrings #-}

import Data.List (transpose)
import Database.SQLite.Simple
import System.Environment (getArgs)

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

filterTense :: [NaturalWord] -> Tense -> [NaturalWord]
filterTense nws t = filter ((== t) . tense) nws

printMood :: [NaturalWord] -> Mood -> IO ()
printMood nws m = do
  putStrLn $ "\n" ++ m ++ "\n"
  mapM_ printRow combined
  where
    moodMatches = filter ((== m) . mood) nws
    tenseValues = concat $ map (filterTense moodMatches) tenses
    personLabels = map fst persons
    mapTenses = flip map tenseValues
    values = map (mapTenses . snd) persons
    annotatedValues = zip personLabels values
    filtered = filter ((any ((/=0) . length)) . snd) annotatedValues
    tenses' = "":tenses
    mergeTuple (label, values') = label:values'
    combined = tenses':(map mergeTuple filtered)
    lengths = map ((foldr1 max) . (map length)) $ (transpose combined)
    printRow row = putStrLn $ formatLine (zip lengths row) 

main :: IO ()
main = do
  verb : _ <- getArgs
  conn <- open "conjugation.db"

  rows <- query conn "SELECT * from verbs where infinitive=?" (Only (verb :: String)) :: IO [NaturalWord]
  mapM_ (printMood rows) moods

  close conn
