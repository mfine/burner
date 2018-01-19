#!/usr/bin/env stack
-- stack runghc --package basic-prelude --package burner --package turtle

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasicPrelude
import Burner
import Turtle

parser :: Parser (Text, Text, Maybe Text, Maybe Int)
parser = (,,,)
  <$> optText "name" 'n' "Instance Name"
  <*> optText "configuration" 'c' "Configuration File"
  <*> optional (optText "kind" 'k' "Instance Type")
  <*> optional (optInt "size" 's' "Disk Size")

main :: IO ()
main = do
  (name, nix, kind, size) <- options "Provision Burner" parser
  burnerProvision name nix (fromMaybe "t2.micro" kind) (fromMaybe 100 size)
