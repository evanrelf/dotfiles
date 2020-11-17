#!/usr/bin/env nix-shell
#!nix-shell -I nixpkgs=nix/.config/nix/nixpkgs.nix
#!nix-shell -p stow "haskellPackages.ghcWithPackages (p: with p; [ directory optparse-applicative process relude string-interpolate ])"
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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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

import Data.String.Interpolate (i)
import Relude

import qualified Data.List.NonEmpty as NonEmpty
import qualified Options.Applicative as Options
import qualified Relude.Extra.Bifunctor as Bifunctor
import qualified System.Directory as Directory
import qualified System.Process as Process


--------------------------------------------------------------------------------
-- RUN
--------------------------------------------------------------------------------


main :: IO ()
main = do
  cliOptions@CliOptions{packages} <- getCliOptions

  existentPackages <- discardNonexistent packages

  mapM_ (usingReaderT cliOptions . install) existentPackages


discardNonexistent :: MonadIO m => NonEmpty FilePath -> m [FilePath]
discardNonexistent packages = do
  (existent, nonexistent) <-
    partitionM (liftIO . Directory.doesDirectoryExist) packages

  forM_ nonexistent \package ->
    putStrLn [i|[#{package}] Configuration doesn't exist|]

  pure existent


install :: (MonadIO m, MonadReader CliOptions m) => FilePath -> m ()
install package = do
  prepare package
  stow package


--------------------------------------------------------------------------------
-- PARSE CLI OPTIONS
--------------------------------------------------------------------------------


data CliOptions = CliOptions
  { packages :: NonEmpty FilePath
  , dryRun :: Bool
  } deriving stock Show


getCliOptions :: MonadIO m => m CliOptions
getCliOptions = liftIO do
  let parserPrefs = Options.prefs Options.showHelpOnError
  let parserInfo = Options.info (Options.helper <*> parseCliOptions) mempty
  Options.customExecParser parserPrefs parserInfo


parseCliOptions :: Options.Parser CliOptions
parseCliOptions = do
  packages <-
    fmap fromList $ some $ Options.strArgument $ mconcat
      [ Options.metavar "PACKAGE"
      ]

  dryRun <-
    Options.switch $ mconcat
      [ Options.long "dry-run"
      , Options.help "Run in dry run mode"
      ]

  pure CliOptions{packages, dryRun}


--------------------------------------------------------------------------------
-- PREPARE
--------------------------------------------------------------------------------


prepare :: (MonadIO m, MonadReader CliOptions m) => FilePath -> m ()
prepare = \case
  "doom" -> prepareEmacs
  "emacs" -> prepareEmacs
  "hammerspoon" -> prepareHammerspoon
  "kakoune" -> prepareKakoune
  "neovim" -> prepareNeovim
  "nix" -> prepareNix
  "tmux" -> prepareTmux
  _ -> pass


prepareEmacs :: (MonadIO m, MonadReader CliOptions m) => m ()
prepareEmacs = do
  putStrLn "[emacs] Setting up truecolor support"
  run "$HOME/dotfiles/emacs/.config/emacs/setup-truecolor"


prepareHammerspoon :: (MonadIO m, MonadReader CliOptions m) => m ()
prepareHammerspoon = do
  undefined


prepareKakoune :: (MonadIO m, MonadReader CliOptions m) => m ()
prepareKakoune = do
  undefined


prepareNeovim :: (MonadIO m, MonadReader CliOptions m) => m ()
prepareNeovim = do
  undefined


prepareNix :: (MonadIO m, MonadReader CliOptions m) => m ()
prepareNix = do
  undefined


prepareTmux :: (MonadIO m, MonadReader CliOptions m) => m ()
prepareTmux = do
  undefined


-- (define/contract (prepare-hammerspoon)
--   (-> any)
--   (printf "[hammerspoon] Changing config file location\n")
--   (check-installed "defaults")
--   (run
--    (string-join
--     '("defaults write org.hammerspoon.Hammerspoon MJConfigFile"
--       "\"$HOME/.config/hammerspoon/init.lua\""))))

-- (define/contract (prepare-kakoune)
--   (-> any)
--   (unless (directory-exists? (home ".config/kak/plugins/plug.kak"))
--     (printf "[kakoune] Installing plug.kak\n")
--     (check-installed "git")
--     (run
--      (string-join
--       '("git clone --depth=1 'https://github.com/robertmeta/plug.kak.git'"
--         "\"$HOME/.config/kak/plugins/plug.kak\"")))))

-- (define/contract (prepare-neovim)
--   (-> any)
--   (unless (file-exists? (home ".local/share/nvim/site/autoload/plug.vim"))
--     (printf "[neovim] Installing vim-plug\n")
--     (check-installed "curl")
--     (run
--      (string-join
--       '("curl --location --fail --create-dirs"
--         "'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'"
--         "-o \"$HOME/.local/share/nvim/site/autoload/plug.vim\"")))))

-- (define/contract (prepare-nix)
--   (-> any)
--   (printf "[nix] Installing profile\n")
--   (check-installed "nix-env")
--   (run
--    (string-join
--     '("nix-env --install"
--       "--file nix/.config/nix/env.nix "
--       "--argstr hostname \"$(hostname)\""))))

-- (define/contract (prepare-tmux)
--   (-> any)
--   (unless (directory-exists? (home ".config/tmux/plugins/tpm"))
--     (printf "[tmux] Installing tpm\n")
--     (check-installed "git")
--     (run
--      (string-join
--       '("git clone --depth=1 'https://github.com/tmux-plugins/tpm.git'"
--         "\"$HOME/.config/tmux/plugins/tpm\"")))))


--------------------------------------------------------------------------------
-- STOW
--------------------------------------------------------------------------------


stow :: (MonadIO m, MonadReader CliOptions m) => FilePath -> m ()
stow package = do
  putStrLn [i|[#{package}] Stowing configuration|]
  assertExecutableExists "stow"
  run [i|stow --stow --target "$HOME" --no-folding #{package}|]


--------------------------------------------------------------------------------
-- HELPER FUNCTIONS
--------------------------------------------------------------------------------


run :: (MonadIO m, MonadReader CliOptions m) => String -> m ()
run command = do
  CliOptions{dryRun} <- ask

  if dryRun
    then putStrLn [i|dry-run> #{command}|]
    else liftIO $ Process.callCommand command


assertExecutableExists :: MonadIO m => FilePath -> m ()
assertExecutableExists executable = liftIO do
  Directory.findExecutable executable `whenNothingM_`
    fail ("Missing " <> executable)


partitionM :: Monad m => (a -> m Bool) -> NonEmpty a -> m ([a], [a])
partitionM predicate xs = do
  xs' <- mapM (\x -> (x, ) <$> predicate x) xs
  pure $ Bifunctor.bimapBoth (fmap fst) (NonEmpty.partition snd xs')
