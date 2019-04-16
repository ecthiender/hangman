{-# LANGUAGE RecordWildCards #-}

module Lib
    ( play
    ) where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Monad          (forever, replicateM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bool              (bool)
import           Data.Char
import           Data.List              (dropWhileEnd, elemIndex, elemIndices,
                                         intercalate, intersperse)
import           Data.Maybe             (fromJust, fromMaybe)
import           Debug.Trace
import           Prelude                hiding (Word)
import           System.IO
import           System.Process         (system)
import           System.Random          (randomRIO)

import qualified Brick                  as B
import qualified Brick.BChan            as B
import qualified Brick.Types            as B
import qualified Graphics.Vty           as V


type Word = String

type Dictionary = [Word]

newtype ObsfucatedWord
  = ObsfucatedWord { obWord :: [Maybe Char] }
  deriving (Eq)

instance Show ObsfucatedWord where
  show = map (fromMaybe '_') . obWord

data GameState
  = GameState
  { gsCurrentWord    :: !Word
  , gsObedWord       :: !ObsfucatedWord
  , gsCurrentChance  :: !Int
  , gsStatus         :: !GameStatus
  , gsGuessedLetters :: ![Char]
  } deriving (Show, Eq)

data GameStatus = Playing | Win | GameOver
  deriving (Show, Eq)
data Guess = Correct Char | Incorrect
  deriving (Show, Eq)

parseDictionary :: String -> Dictionary
parseDictionary txt =
  map (map toUpper . trim) $ filter (\w -> length w > 3) $ lines txt
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace

getRandomWord :: Dictionary -> IO Word
getRandomWord dict = do
  randomIdx <- randomRIO (0, length dict)
  return $ dict !! randomIdx

obsfucateWord :: Word -> IO ObsfucatedWord
obsfucateWord w = do
  -- TODO: better way of doing this?
  rIdxs <- replicateM halfLen (randomRIO (0, length w))
  let ob = map (\c -> bool (Just c) Nothing $ shouldHide rIdxs c) w
  return $ ObsfucatedWord ob
  where
    shouldHide rIdxs c = fromJust (elemIndex c w) `elem` rIdxs
    halfLen = length w `div` 2

chances :: Int
chances = 6

incorrectGuess :: Char -> GameState -> GameState
incorrectGuess guess gs = gs { gsCurrentChance = gsCurrentChance gs + 1
                             , gsStatus = status
                             , gsGuessedLetters = guess:(gsGuessedLetters gs)
                             }
  where status
          | gsCurrentChance gs == chances - 1 = GameOver
          | otherwise = Playing

checkGuess :: GameState -> Char -> Guess
checkGuess gs guess =
  bool Incorrect (Correct guess) $ guess `elem` cword
  where
    cword = gsCurrentWord gs

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b)
  where (a, _:b) = splitAt n ls

reveal :: GameState -> Char -> GameState
reveal gs letter =
  if show newobword == cword
  then gs { gsStatus = Win, gsObedWord = newobword }
  else gs { gsObedWord = newobword }
  where
    newobword = ObsfucatedWord $ foldl
                (\w x -> replaceAtIndex x (Just letter) w)
                obedWord idxs
    idxs = elemIndices letter cword
    obedWord = obWord $ gsObedWord gs
    cword = gsCurrentWord gs


type Name = ()

data Tick = Tick

draw :: GameState -> [B.Widget Name]
draw gs@GameState {..} =
  [ drawMain B.<=> drawStats]
  where
    drawMain = B.padAll 5 $ B.str (show gsObedWord)
    drawStats =
      B.hBox [ B.str $ "REMAINING CHANCES: " ++
               show (chances - gsCurrentChance) ++ "  "
            , B.str $ "GUESSED LETTERS: " ++ intersperse ',' gsGuessedLetters
            ]

handleEvent :: GameState -> B.BrickEvent Name Tick -> B.EventM Name (B.Next GameState)
handleEvent s ev =
  case gsStatus s of
    Win      -> B.halt s
    GameOver -> B.halt s
    Playing  -> case ev of
      (B.AppEvent Tick)                              -> B.continue s
      (B.VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) -> B.halt s
      (B.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) -> B.halt s
      (B.VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) -> B.halt s
      (B.VtyEvent (V.EvKey (V.KChar ch) []))         ->
        B.continue $ handleGuess $ toUpper ch
      _                                              -> B.continue s

  where
    handleGuess guess =
      case checkGuess s guess of
        Correct ch -> reveal s ch
        Incorrect  -> incorrectGuess guess s

app :: B.App GameState Tick Name
app = B.App { B.appDraw = draw
            , B.appChooseCursor = B.neverShowCursor
            , B.appHandleEvent = handleEvent
            , B.appStartEvent = return
            , B.appAttrMap = const theMap
            }

theMap :: B.AttrMap
theMap = B.attrMap mempty []

play :: IO ()
play = do
  chan <- B.newBChan 10
  sendTicks chan
  initialState <- mkInitialState
  initVty <- buildVty
  finalState <- B.customMain initVty buildVty (Just chan) app initialState
  case gsStatus finalState of
    Win -> do
      putStrLn " Y O U  W I N ! !"
      putStrLn ""
      putStrLn $ "WORD WAS: " ++ gsCurrentWord finalState
    GameOver -> do
      putStrLn " YOU LOSE PANSY! !"
      putStrLn ""
      putStrLn $ "WORD WAS: " ++ gsCurrentWord finalState
    Playing -> putStrLn "Oops! Some this is not supposed to happen"

  where
    sendTicks chan =
      forkIO $ forever $ do
        B.writeBChan chan Tick
        threadDelay 1000000
    buildVty = V.mkVty V.defaultConfig

mkInitialState :: IO GameState
mkInitialState =  do
  contents <- readFile "data/nounlist.txt"
  let dict = parseDictionary contents
  randomWord <- getRandomWord dict
  obedWord <- obsfucateWord randomWord
  return $ GameState randomWord obedWord 0 Playing []
