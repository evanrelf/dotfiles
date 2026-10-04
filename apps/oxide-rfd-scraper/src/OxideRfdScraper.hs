{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- TODO: Remove
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module OxideRfdScraper (main) where

import Database.SQLite.Simple qualified as SQLite
import Options.Applicative qualified as Optparse
import Prelude hiding (getArgs)
import Text.HTML.Scalpel qualified as Scalpel
import UnliftIO.Directory qualified as Directory
import UnliftIO.Exception qualified as Exception
import Web.Scotty (scotty)
import Web.Scotty qualified as Scotty

main :: IO ()
main = do
  env <- getEnv
  args <- getArgs

  -- TODO: Run database migrations

  case args.command of
    Command_Scrape scrapeArgs ->
      let config = mkScrapeConfig env scrapeArgs
      in runScrape config
    Command_Syndicate syndicateArgs ->
      let config = mkSyndicateConfig env syndicateArgs
      in runSyndicate config

-- SYNDICATION -----------------------------------------------------------------

runSyndicate :: SyndicateConfig -> IO ()
runSyndicate config = do
  putTextLn ("Listening on http://localhost:" <> show config.port)
  scotty config.port do
    Scotty.get "/" do
      Scotty.html "<h1>Hello, world!</h1>"
    Scotty.get "/feed.json" do
      -- TODO: Serve JSON Feed
      Scotty.json (Nothing :: Maybe ())

-- SCRAPING --------------------------------------------------------------------

runScrape :: ScrapeConfig -> IO ()
runScrape _config = do
  undefined

-- DATABASE MIGRATIONS ---------------------------------------------------------

-- TODO

-- CONFIGURATION ---------------------------------------------------------------

data SyndicateConfig = SyndicateConfig
  { port :: Int
  , invocationId :: Text
  , stateDirectory :: FilePath
  }

data ScrapeConfig = ScrapeConfig
  { invocationId :: Text
  , stateDirectory :: FilePath
  }

mkSyndicateConfig :: Env -> SyndicateArgs -> SyndicateConfig
mkSyndicateConfig env args = do
  let port = fromMaybe 3000 (args.port <|> env.port)
  let invocationId = fromMaybe "NONE" env.invocationId
  let stateDirectory = fromMaybe "." env.stateDirectory
  SyndicateConfig{ port, invocationId, stateDirectory }

mkScrapeConfig :: Env -> ScrapeArgs -> ScrapeConfig
mkScrapeConfig env args = do
  let invocationId = fromMaybe "NONE" env.invocationId
  let stateDirectory = fromMaybe "." env.stateDirectory
  ScrapeConfig{ invocationId, stateDirectory }

-- ENVIRONMENT VARIABLES -------------------------------------------------------

data Env = Env
  { port :: Maybe Int
  , invocationId :: Maybe Text
  , stateDirectory :: Maybe FilePath
  }

getEnv :: IO Env
getEnv = do
  port <- do
    mString <- lookupEnv "PORT"
    case mString of
      Nothing -> pure Nothing
      Just string ->
        case readEither string of
          Left _error -> Exception.throwString "Failed to parse `PORT` as `Int`"
          Right port -> pure $ Just port

  invocationId <- do
    mString <- lookupEnv "INVOCATION_ID"
    case mString of
      Nothing -> pure Nothing
      Just string -> pure $ Just (toText string)

  stateDirectory <- do
    mString <- lookupEnv "STATE_DIRECTORY"
    case mString of
      Nothing -> pure Nothing
      Just string -> do
        exists <- Directory.doesDirectoryExist string
        if not exists
          then Exception.throwString "`STATE_DIRECTORY` does not exist"
          else pure $ Just string

  pure Env{ port, invocationId, stateDirectory }

-- COMMAND-LINE ARGUMENTS ------------------------------------------------------

data Args = Args
  { command :: Command
  }

data Command
  = Command_Syndicate SyndicateArgs
  | Command_Scrape ScrapeArgs

data SyndicateArgs = SyndicateArgs
  { port :: Maybe Int
  }

data ScrapeArgs = ScrapeArgs
  {
  }

parseSyndicateArgs :: Optparse.Parser SyndicateArgs
parseSyndicateArgs = do
  port <- Optparse.optional $ Optparse.option Optparse.auto $ mconcat
    [ Optparse.long "port"
    , Optparse.metavar "NUMBER"
    ]
  pure SyndicateArgs{ port }

parseScrapeArgs :: Optparse.Parser ScrapeArgs
parseScrapeArgs = do
  pure ScrapeArgs{}

parseArgs :: Optparse.Parser Args
parseArgs = do
  command <- Optparse.hsubparser $ mconcat
    [ Optparse.command "syndicate" $
        Optparse.info
          (fmap Command_Syndicate parseSyndicateArgs)
          (Optparse.progDesc "Serve Oxide RFDs as a JSON Feed")
    , Optparse.command "scrape" $
        Optparse.info
          (fmap Command_Scrape parseScrapeArgs)
          (Optparse.progDesc "Scrape the Oxide RFDs website")
    ]
  pure Args{ command }

getArgs :: IO Args
getArgs = do
  let parserPrefs = Optparse.prefs mempty
  let parserInfo = Optparse.info (Optparse.helper <*> parseArgs) mempty
  Optparse.customExecParser parserPrefs parserInfo
