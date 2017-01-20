#!/usr/bin/env stack
-- stack runghc --package basic-prelude --package burner --package turtle

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasicPrelude
import Burner
import Turtle

parser :: Parser Text
parser = optText "id" 'i' "Instance Id"

main :: IO ()
main = do
  ids <- options "Burner Status" parser
  out <- burnerStatus ids
  putStrLn out
