{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Burner EC2 Instances
--
module Burner
  ( burnerCreate
  , burnerState
  , burnerAddress
  , burnerStatus
  ) where

import BasicPrelude hiding (empty)
import Turtle

-- | procStrict wrapper that throws exceptions.
--
process :: MonadIO m => Text -> [Text] -> Shell Text -> m Text
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
  , "--image-id", "ami-49fe2729"
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
jq :: MonadIO m => [Text] -> Shell Text -> m Text
jq args i = head . lines <$> process "jq" ("-rc" : args) i

-- | Create burner.
--
burnerCreate :: MonadIO m => Text -> Int -> m Text
burnerCreate kind size = do
  out <- ec2Run kind size
  jq [ ".Instances[] | .InstanceId" ] $ pure out

-- | State of burner.
--
burnerState :: MonadIO m => Text -> m Text
burnerState ids = do
  out <- ec2Describe ids
  jq [ ".Reservations[].Instances[].State.Name" ] $ pure out

-- | Address of burner.
--
burnerAddress :: MonadIO m => Text -> m Text
burnerAddress ids = do
  out <- ec2Describe ids
  jq [ ".Reservations[].Instances[].PublicIpAddress" ] $ pure out

-- | Status of burner.
--
burnerStatus :: MonadIO m => Text -> m Text
burnerStatus ids = do
  out <- ec2DescribeStatus ids
  jq [ ".InstanceStatuses[].InstanceStatus.Status" ] $ pure out
