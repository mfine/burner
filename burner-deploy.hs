#!/usr/bin/env stack
-- stack runghc --package basic-prelude --package burner --package turtle

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasicPrelude
import Burner
import Turtle

parser :: Parser (Text, Text)
parser = (,)
  <$> optText "name" 'n' "Instance Name"
  <*> optText "configuration" 'c' "Configuration File"

main :: IO ()
main = do
  (name, nix) <- options "Deploy Burner" parser
  burnerDeploy name nix
