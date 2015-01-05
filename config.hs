{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Text
import Data.Configurator

data Conf = Conf {port :: Int , dbName :: Text , serverName :: Text}

cfgName = "settings.cfg"

get = do
	cfg        <- load [Required cfgName]
	port       <- require cfg "port"
	dbName     <- require cfg "dbName"
	serverName <- require cfg "serverName"
	return (Conf port dbName serverName)