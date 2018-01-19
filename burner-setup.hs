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
  <*> optText "id" 'i' "Instance Id"

main :: IO ()
main = do
  (name, ids) <- options "Burner Setup" parser
  burnerSetup name ids
