{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-
	Stanislav Ursache - 9.1.2015
	spock 0.7.6.0
-}

module Main where

import Model

import qualified Get
import qualified Update
import qualified Config as C
import qualified Database as DB

import Data.Text
import Web.Spock.Simple

main =
	C.get >>= \conf ->
		DB.makeDB (C.dbName conf) >>= \pool ->
			runSpock (C.port conf) $ spock dCfg (PCPool pool) (conf) run
	where
		dCfg = SessionCfg "def" (0) 42 id Nothing

run = do
	anyPage
	post         "/reg"     $ auth newPlayer

	subcomponent "/get"     $ do
		get      "/top"     $      Get.top
		get      "/ctop"    $      Get.cTop
		post     "/rank"    $ auth Get.rank
		post     "/crank"   $ auth Get.cRank
		post     "/player"  $ auth Get.player
		post     "/rplayer" $ auth Get.rPlayer

	subcomponent "/update"  $ do
		post     "/name"    $ auth Update.name
		post     "/score"   $ auth Update.score
		post     "/cscore"  $ auth Update.cScore

{- PAGES-}

anyPage = do
	hookAny GET  mainPage
	hookAny POST mainPage
	where
		mainPage eText = getState >>= \conf ->
			html $ append (C.servName conf) (C.servInfo conf)

newPlayer = do
	(Just name :: Maybe Text) <- param k_playerName

	rawID <- DB.insertPerson (newPerson name)
	DB.suId rawID
	json $ DB.keyOutLB rawID
