{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Get where

import qualified Database as DB

import Model
import Web.Spock.Simple

player =
	param k_playerID >>= \(Just pID :: Maybe Int) ->
		DB.getByID pID >>= \out                   ->
			json $ DB.extract out

rPlayer =
	param k_playerID >>= \(Just pID :: Maybe Int) ->
		DB.getPlayerRandom pID >>= \out           ->
			json $ (DB.extract out :: [Person])

top =
	DB.getTop >>= \out ->
		json $ DB.extract out

cTop =
	DB.getTopChallenge >>= \out ->
		json $ DB.extract out

rank = do
	param k_score >>= \(Just score :: Maybe Int) ->
		 DB.getPlayerRank (score + 1) >>= \count ->
		 	json $ [count :: Int]

cRank =
	param k_CScore >>= \(Just score :: Maybe Int) ->
		DB.getPlayerRankC (score + 1) >>= \count  ->
			json $ [count :: Int]
