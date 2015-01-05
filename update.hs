{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Update where

import qualified Database as DB

import Model
import Data.Text
import Web.Spock.Simple

name = do
	(Just pID :: Maybe Int) <- param k_playerID
	(Just nName :: Maybe Text) <- param k_playerName

	DB.uName pID nName
	jSucces

score = do 
	(Just pID :: Maybe Int) <- param k_playerID
	(Just score :: Maybe Int) <- param k_score

	DB.uScore pID score

	playerRank <- DB.getPlayerRank (score + 1)
	json $ [playerRank :: Int]

cScore = do 
	(Just pID :: Maybe Int) <- param k_playerID

	DB.uCScore pID