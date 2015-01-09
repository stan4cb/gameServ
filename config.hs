{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Text
import Data.Configurator

data Conf = Conf 
	{ port       :: Int 
	, dbName     :: Text
	, serverName :: Text
	, user       :: Text
	, pass       :: Text
	}

cfgName = "settings.cfg"

get = 
	load [Required cfgName] >>= \cfg -> do
		port       <- require cfg "port"
		dbName     <- require cfg "dbName"
		serverName <- require cfg "serverName"
		user       <- require cfg "auth_user"
		pass       <- require cfg "auth_pass"
		return (Conf port dbName serverName user pass)