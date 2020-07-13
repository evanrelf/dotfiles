#!/usr/bin/env nix-shell
#!nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [ directory extra filepath optparse-applicative process relude ])" -i runghc

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wall -Werror #-}

import Relude

import Control.Monad.Extra (partitionM)
import Optparse.Applicative
import qualified System.Directory as Directory
import System.Exit (exitFailure)
import System.FilePath ((</>))
import qualified System.Process as Process


run :: String -> IO ()
run command =
  if dryRun
    then putStrLn ("dry-run> " <> command)
    else Process.callCommand command


checkInstalled :: String -> IO ()
checkInstalled executable =
  whenNothingM_ (Directory.findExecutable executable) do
    putStrLn ("Missing executable: " <> executable)
    exitFailure


discardNonexistent :: [String] -> IO [String]
discardNonexistent packages = do
  (existent, nonexistent) <- partitionM Directory.doesDirectoryExist packages
  mapM_ (\x -> putStrLn ("[" <> x <> "] Configuration doesn't exist")) nonexistent
  pure existent


prepare :: String -> IO ()
prepare package = do
  home <- Directory.getHomeDirectory

  case package of
    "hammerspoon" -> do
      putStrLn "[hammerspoon] Changing config file location"
      run $ unwords
        [ "defaults write org.hammerspoon.Hammerspoon MJConfigFile"
        , "\"$HOME/.config/hammerspoon/init.lua\""
        ]

    "kakoune" ->
      unless (Directory.doesDirectoryExist (home </> ".config/kak/plugins/plug.kak")) do
        putStrLn "[kakoune] Installing plug.kak"
        checkInstalled "git"
        run $ unwords
          [ "git clone --depth=1 https://github.com/andreyorst/plug.kak.git"
          , "\"$HOME/.config/kak/plugins/plug.kak\""
          ]

    "neovim" ->
      unless (Directory.doesFileExist (home </> ".local/share/nvim/site/autoload/plug.vim")) do
        putStrLn "[neovim] Installing vim-plug"
        checkInstalled "curl"
        run $ unwords
          [ "curl --location --fail --create-dirs"
          , "'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'"
          , "--output=\"$HOME/.local/share/nvim/site/autoload/plug.vim\""
          ]

    "tmux" ->
      unless (Directory.doesDirectoryExist (home </> ".config/tmux/plugins/tpm")) do
        checkInstalled "git"
        run $ unwords
          [ "git clone --depth=1 'https://github.com/tmux-plugins/tpm.git'"
          , "\"$HOME/.config/tmux/plugins/tpm\""
          ]

    _ -> pass


stow :: String -> IO ()
stow package = do
  putStrLn ("[" <> package <> "] Stowing configuration")
  checkInstalled "stow"
  run ("stow --stow --target=\"$HOME\" --no-folding " <> package)


install :: String -> IO ()
install package = do
  prepare package
  stow package


main :: IO ()
main = do
  undefined -- TODO: Command-line options parser

  when (null packages) do
    putStrLn "No packages specified"
    exitFailure

  mapM_ install (discardNonexistent packages)
