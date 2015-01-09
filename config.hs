{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Text
import Data.Configurator

data Conf = Conf 
	{ port     :: Int 
	, dbName   :: Text
	, servName :: Text
	, servInfo :: Text
	, user     :: Text
	, pass     :: Text
	}

cfgName = "settings.cfg"

get = 
	load [Required cfgName] >>= \cfg -> do
		port     <- require cfg "port"
		dbName   <- require cfg "dbName"
		servName <- require cfg "servName"
		servInfo <- require cfg "servInfo"
		user     <- require cfg "auth_user"
		pass     <- require cfg "auth_pass"
		return (Conf port dbName servName servInfo user pass)