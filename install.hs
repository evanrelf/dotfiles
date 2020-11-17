#!/usr/bin/env nix-shell
#!nix-shell -I nixpkgs=nix/.config/nix/pkgs.nix
#!nix-shell -p stow "haskellPackages.ghcWithPackages (p: with p; [ directory optparse-applicative process relude string-interpolate ])"
#!nix-shell -i runghc

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

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
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.IO.Unsafe as IO.Unsafe
import qualified System.Process as Process


--------------------------------------------------------------------------------
-- RUN
--------------------------------------------------------------------------------


main :: IO ()
main = do
  rawOptions@Options{packages = rawPackages} <- getOptions

  let stripSlashes = filter (/= '/')

  existentPackages <- discardNonexistent (fmap stripSlashes rawPackages)

  case nonEmpty existentPackages of
    Nothing ->
      die "No packages specified"

    Just packages -> do
      let options = rawOptions{packages}

      mapM_ (usingReaderT options . install) packages


discardNonexistent :: MonadIO m => NonEmpty FilePath -> m [FilePath]
discardNonexistent packages = do
  (existent, nonexistent) <-
    partitionM (liftIO . Directory.doesDirectoryExist) packages

  forM_ nonexistent \package ->
    putStrLn [i|[#{package}] Configuration doesn't exist|]

  pure existent


install :: (MonadIO m, MonadReader Options m) => FilePath -> m ()
install package = do
  prepare package
  stow package


--------------------------------------------------------------------------------
-- PARSE CLI OPTIONS
--------------------------------------------------------------------------------


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


--------------------------------------------------------------------------------
-- PREPARE PACKAGES
--------------------------------------------------------------------------------


prepare :: (MonadIO m, MonadReader Options m) => FilePath -> m ()
prepare = \case
  "doom" -> prepareEmacs
  "emacs" -> prepareEmacs
  "hammerspoon" -> prepareHammerspoon
  "kakoune" -> prepareKakoune
  "neovim" -> prepareNeovim
  "nix" -> prepareNix
  "tmux" -> prepareTmux
  _ -> pass


prepareEmacs :: (MonadIO m, MonadReader Options m) => m ()
prepareEmacs = do
  putStrLn "[emacs] Setting up truecolor support"

  run "$HOME/dotfiles/emacs/.config/emacs/setup-truecolor"


prepareHammerspoon :: (MonadIO m, MonadReader Options m) => m ()
prepareHammerspoon = do
  putStrLn "[hammerspoon] Changing config file location"

  checkInstalled "defaults"

  run [iii|
    defaults write org.hammerspoon.Hammerspoon MJConfigFile
      "$HOME/.config/hammerspoon/init.lua"
  |]


prepareKakoune :: (MonadIO m, MonadReader Options m) => m ()
prepareKakoune = do
  let plugKak = [i|#{home}/.config/kak/plugins/plug.kak|]

  unlessM (liftIO $ Directory.doesDirectoryExist plugKak) do
    putStrLn "[kakoune] Installing plug.kak"

    checkInstalled "git"

    run [iii|
      git clone --depth=1 "https://github.com/robertmeta/plug.kak.git"
        "$HOME/.config/kak/plugins/plug.kak"
    |]


prepareNeovim :: (MonadIO m, MonadReader Options m) => m ()
prepareNeovim = do
  let plugVim = [i|#{home}/.local/share/nvim/site/autoload/plug.vim|]

  unlessM (liftIO $ Directory.doesFileExist plugVim) do
    putStrLn "Installing vim-plug"

    checkInstalled "curl"

    run [iii|
      curl --location --fail --create-dirs
        "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
        -o "$HOME/.local/share/nvim/site/autoload/plug.vim"
    |]


prepareNix :: (MonadIO m, MonadReader Options m) => m ()
prepareNix = do
  putStrLn "[nix] Installing profile"

  checkInstalled "nix-env"

  run [iii|
    nix-env
      --install
      --file nix/.config/nix/env.nix
      --argstr hostname "$(hostname -s)"
  |]


prepareTmux :: (MonadIO m, MonadReader Options m) => m ()
prepareTmux = do
  let pluginsDirectory = [i|#{home}/.config/tmux/plugins/tpm|]

  unlessM (liftIO $ Directory.doesDirectoryExist pluginsDirectory) do
    putStrLn "[tmux] Installing tpm"

    checkInstalled "git"

    run [iii|
      git clone --depth=1 "https://github.com/tmux-plugins/tpm.git"
        "$HOME/.config/tmux/plugins/tpm"
    |]


--------------------------------------------------------------------------------
-- STOW PACKAGES' CONFIGURATIONS
--------------------------------------------------------------------------------


stow :: (MonadIO m, MonadReader Options m) => FilePath -> m ()
stow package = do
  putStrLn [i|[#{package}] Stowing configuration|]

  checkInstalled "stow"

  run [i|stow --stow --target "$HOME" --no-folding #{package}|]


--------------------------------------------------------------------------------
-- HELPER FUNCTIONS
--------------------------------------------------------------------------------


{-# NOINLINE home #-}
home :: FilePath
home = IO.Unsafe.unsafePerformIO do
  Environment.lookupEnv "HOME" `whenNothingM`
    fail "Failed to get HOME environment variable"


run :: (MonadIO m, MonadReader Options m) => String -> m ()
run command = do
  Options{dryRun} <- ask

  if dryRun then
    putStrLn [i|dry-run> #{command}|]

  else
    liftIO $ Process.callCommand command


checkInstalled :: MonadIO m => FilePath -> m ()
checkInstalled executable = liftIO do
  Directory.findExecutable executable `whenNothingM_`
    fail [i|Missing executable: #{executable}|]


partitionM :: Monad m => (a -> m Bool) -> NonEmpty a -> m ([a], [a])
partitionM predicate xs = do
  xs' <- mapM (\x -> (x, ) <$> predicate x) xs
  pure $ Bifunctor.bimapBoth (fmap fst) (NonEmpty.partition snd xs')
