{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Burner EC2 Instances
--
module Burner
  ( burnerCreate
  , burnerState
  , burnerAddress
  , burnerStatus
  , burnerSetup
  , burnerDeploy
  , burnerProvision
  ) where

import BasicPrelude      hiding (empty, isInfixOf, (</>))
import Control.Exception
import Data.Text         hiding (empty, head)
import NeatInterpolation
import Turtle            hiding (text)

-- | procStrict wrapper that throws exceptions.
--
process :: MonadIO m => Text -> [Text] -> Shell Line -> m Text
process cmd args i = do
  (ec, out) <- procStrict cmd args i
  case ec of
    ExitSuccess -> return out
    _           -> liftIO $ throwIO $ ProcFailed cmd args ec

-- | aws command.
--
aws :: MonadIO m => Text -> [Text] -> m Text
aws cmd args = process "aws" (cmd : args) empty

-- | aws ec2 command.
--
ec2 :: MonadIO m => Text -> [Text] -> m Text
ec2 cmd args = aws "ec2" (cmd : "--output" : "json" : args)

-- | aws ec2 run-instances command.
--
ec2Run :: MonadIO m => Text -> Int -> m Text
ec2Run kind size = ec2 "run-instances"
  [ "--count", "1"
  , "--image-id", "ami-3eb40346"
  , "--instance-type", kind
  , "--key-name", "mark"
  , "--security-groups", "launch-wizard-15"
  , "--block-device-mappings", format ("DeviceName=/dev/sda1,Ebs={VolumeSize=" % d % ",DeleteOnTermination=true}") size
  ]

-- | aws ec2 describe-instances command.
--
ec2Describe :: MonadIO m => Text -> m Text
ec2Describe ids = ec2 "describe-instances"
  [ "--instance-ids", ids
  ]

-- | aws ec2 describe-instance-status command.
--
ec2DescribeStatus :: MonadIO m => Text -> m Text
ec2DescribeStatus ids = ec2 "describe-instance-status"
  [ "--instance-ids", ids
  ]

-- | jq command.
--
jq :: MonadIO m => [Text] -> Shell Line -> m Text
jq args i = head . lines <$> process "jq" ("-rc" : args) i

-- | ssh command.
--
ssh :: MonadIO m => [Text] -> m ()
ssh args = void $ proc "ssh" args empty

-- | scp command.
--
scp :: MonadIO m => [Text] -> m ()
scp args = void $ proc "scp" args empty

-- | Wait loop
--
loop :: MonadIO m => m Bool -> m ()
loop action = do
  ok <- action
  unless ok $ do
    sleep 5
    loop action

-- | Create burner.
--
burnerCreate :: MonadIO m => Text -> Int -> m Text
burnerCreate kind size =
  ec2Run kind size >>=
    jq [ ".Instances[] | .InstanceId" ] . pure . unsafeTextToLine

-- | State of burner.
--
burnerState :: MonadIO m => Text -> m Text
burnerState ids =
  ec2Describe ids >>=
    jq [ ".Reservations[].Instances[].State.Name" ] . pure . unsafeTextToLine

-- | Address of burner.
--
burnerAddress :: MonadIO m => Text -> m Text
burnerAddress ids =
  ec2Describe ids >>=
    jq [ ".Reservations[].Instances[].PublicIpAddress" ] . pure . unsafeTextToLine

-- | Status of burner.
--
burnerStatus :: MonadIO m => Text -> m Text
burnerStatus ids =
  ec2DescribeStatus ids >>=
    jq [ ".InstanceStatuses[].InstanceStatus.Status" ] . pure . unsafeTextToLine

-- | Setup burner.
--
burnerSetup :: MonadIO m => Text -> Text -> m ()
burnerSetup name ids = do
  echo "Checking state"
  loop $ ("running" == ) <$> burnerState ids
  echo "Checking status"
  loop $ ("ok" ==) <$> burnerStatus ids
  echo "Getting address"
  ip     <- burnerAddress ids
  echo "Reading SSH configuration"
  file   <- (</> ".ssh/config") <$> home
  config <- liftIO $ readTextFile file
  unless (ip `isInfixOf` config) $ do
    echo "Writing SSH configuration"
    liftIO $ writeTextFile file $ config <> configText ip
  where
    configText ip = [text|
Host $name+root
  Hostname $ip
  User root
  IdentityFile ~/.ssh/mark.pem

Host $name
  Hostname $ip
  User mark

|]

-- | Deploy configuration.
--
burnerDeploy :: MonadIO m => Text -> Text -> m ()
burnerDeploy name nix = do
  echo "Copying configuration"
  scp [ "-q", nix, name <> "+root:/etc/nixos/configuration.nix" ]
  echo "Deploying configuration"
  ssh [ name <> "+root", "nixos-rebuild", "switch" ]

-- | Provision burner.
--
burnerProvision :: MonadIO m => Text -> Text -> Text -> Int -> m ()
burnerProvision name nix kind size = do
  ids <- burnerCreate kind size
  burnerSetup name ids
  burnerDeploy name nix
