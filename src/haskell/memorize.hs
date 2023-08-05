{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Options.Applicative qualified as Options
import Relude
import System.Console.ANSI qualified as Ansi

data Options = Options
  { mode :: Mode
  }

data Mode
  = Distance
  | Temperature
  | Time

parseOptions :: Options.Parser Options
parseOptions = undefined

getOptions :: IO Options
getOptions = undefined

main :: IO ()
main = do
  options <- getOptions
  pure ()

prompt :: Text -> IO Text
prompt message = do
  putText $ message <> " "
  hFlush stdout
  getLine

eraseLine :: IO ()
eraseLine = do
  Ansi.cursorUpLine 1
  Ansi.clearLine

-- (x: f32) -> f32
convertKmToMi :: ()
convertKmToMi = undefined

-- (x: f32) -> f32
convertMiToKm :: ()
convertMiToKm = undefined

-- (x: f32) -> f32
convertCToF :: ()
convertCToF = undefined

-- (x: f32) -> f32
convertFToC :: ()
convertFToC = undefined

-- (x: u8) -> (u8, MeridiemIndicator)
convert24hTo12h :: ()
convert24hTo12h = undefined

-- (x: (u8, MeridiemIndicator)) -> u8
convert12hTo24h :: ()
convert12hTo24h = undefined

-- () -> f32
genKm :: ()
genKm = undefined

-- () -> f32
genMi :: ()
genMi = undefined

-- () -> f32
genC :: ()
genC = undefined

-- () -> f32
genF :: ()
genF = undefined

-- () -> u8
gen24h :: ()
gen24h = undefined

-- () -> (u8, MeridiemIndicator)
gen12h :: ()
gen12h = undefined
