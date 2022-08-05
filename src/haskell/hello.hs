#!/usr/bin/env cached-nix-shell
#!nix-shell -p "ghc.withPackages (p: with p; [ relude ])"
#!nix-shell -i runghc

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}

import Relude

main :: IO ()
main = do
  putTextLn "Hello, world!"
