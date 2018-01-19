#!/usr/bin/env stack
-- stack runghc --package basic-prelude --package burner --package turtle

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasicPrelude
import Burner
import Turtle

parser :: Parser (Maybe Text, Maybe Int)
parser = (,)
  <$> optional (optText "kind" 'k' "Instance Type")
  <*> optional (optInt "size" 's' "Disk Size")

main :: IO ()
main = do
  (kind, size) <- options "Create Burner" parser
  out <- burnerCreate (fromMaybe "t2.micro" kind) (fromMaybe 100 size)
  putStrLn out
