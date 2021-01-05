#!/usr/bin/env nix-shell
#!nix-shell -I nixpkgs=nix/.config/nix/pkgs.nix
#!nix-shell -p stow "haskellPackages.ghcWithPackages (p: with p; [ ansi-terminal optparse-applicative relude string-interpolate unliftio ])"
#!nix-shell -i runghc

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Werror=incomplete-record-updates #-}
{-# OPTIONS_GHC -Werror=incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}
{-# OPTIONS_GHC -Werror=partial-fields #-}
{-# OPTIONS_GHC -Widentities #-}
{-# OPTIONS_GHC -Wmissing-home-modules #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# OPTIONS_GHC -fshow-warning-groups #-}
{-# OPTIONS_GHC -threaded #-}
{-# OPTIONS_GHC -rtsopts #-}
{-# OPTIONS_GHC -with-rtsopts=-N #-}

module Main (main) where

import Data.String.Interpolate (i, iii)
import Relude

import qualified Data.List.NonEmpty as NonEmpty
import qualified Options.Applicative as Options
import qualified Relude.Extra.Bifunctor as Bifunctor
import qualified System.Console.ANSI as Ansi
import qualified UnliftIO.Directory as Directory
import qualified UnliftIO.Process as Process


main :: IO ()
main = do
  Options{packages = rawPackages, dryRun} <- getOptions

  let stripSlashes = filter (/= '/')

  existentPackages <- discardNonexistent (fmap stripSlashes rawPackages)

  case nonEmpty existentPackages of
    Nothing -> do
      putStrLnColored Ansi.Red "No packages specified"
      exitFailure

    Just packages ->
      mapM_ (usingReaderT dryRun . install) packages


data Options = Options
  { packages :: NonEmpty FilePath
  , dryRun :: Bool
  } deriving stock Show


getOptions :: MonadIO m => m Options
getOptions = liftIO do
  let parserPrefs = Options.prefs Options.showHelpOnError
  let parserInfo = Options.info (Options.helper <*> parseOptions) mempty
  Options.customExecParser parserPrefs parserInfo


parseOptions :: Options.Parser Options
parseOptions = do
  packages <-
    fmap fromList $ some $ Options.strArgument $ mconcat
      [ Options.metavar "PACKAGE"
      ]

  dryRun <-
    Options.switch $ mconcat
      [ Options.long "dry-run"
      , Options.help "Run in dry run mode"
      ]

  pure Options{packages, dryRun}


discardNonexistent :: MonadIO m => NonEmpty FilePath -> m [FilePath]
discardNonexistent packages = do
  (existent, nonexistent) <- partitionM Directory.doesDirectoryExist packages

  forM_ nonexistent \package ->
    putStrLnColored Ansi.Yellow [i|[#{package}] Configuration doesn't exist|]

  pure existent


partitionM :: Monad m => (a -> m Bool) -> NonEmpty a -> m ([a], [a])
partitionM predicate xs = do
  xs' <- mapM (\x -> (x, ) <$> predicate x) xs
  pure $ Bifunctor.bimapBoth (fmap fst) (NonEmpty.partition snd xs')


install :: (MonadIO m, MonadReader Bool m) => FilePath -> m ()
install package =
  mapM_
    ($ package)
    [ runHook "before"
    , stow
    , runHook "after"
    ]


runHook :: (MonadIO m, MonadReader Bool m) => String -> FilePath -> m ()
runHook hookName package = do
  let script = [i|#{package}/#{hookName}-hook|]

  whenM (Directory.doesFileExist script) do
    log [i|[#{package}] Running #{hookName} hook|]

    sh [i|./#{script}|]


stow :: (MonadIO m, MonadReader Bool m) => FilePath -> m ()
stow package = do
  log [i|[#{package}] Stowing configuration|]

  sh [iii|
    stow
      --stow
      --target "$HOME"
      --no-folding #{package}
      --ignore "-hook"
  |]


sh :: (MonadIO m, MonadReader Bool m) => String -> m ()
sh command = do
  dryRun <- ask

  if dryRun then
    log [i|dry-run> #{command}|]

  else do
    log [i|+ #{command}|]
    Process.callCommand command


log :: MonadIO m => String -> m ()
log = putStrLnColored Ansi.Magenta


putStrLnColored :: MonadIO m => Ansi.Color -> String -> m ()
putStrLnColored color message = do
  let colored = Ansi.setSGRCode [Ansi.SetColor Ansi.Foreground Ansi.Dull color]
  let reset = Ansi.setSGRCode [Ansi.Reset]
  putStrLn (colored <> message <> reset)
