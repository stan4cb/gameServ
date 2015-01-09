{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}

module Database where

import Model
import Database.Persist.Sqlite

import Data.Text hiding (count)
import Web.Spock.Simple hiding (delete)

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)

{- --- -}

initDB     nm = runSqlite nm $ runMigration migrateAll
createPool nm = runNoLoggingT $ createSqlitePool nm 10
lockDB p   = runSqlPool (selectFirst [PersonId ==. (toKeyP (0 :: Int))] []) p

makeDB db = initDB db >>
	createPool db >>= \p ->
		lockDB p >>
		return p

{-
runIOPool action = do
	(pool,_,_) <- getState
	liftIO $ runSqlPool action pool
-}

runIOPool action = 
	runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn

sList exp         = runIOPool $ selectList [exp] []
uDB key wKey nVal = runIOPool $ update key [wKey =. nVal]

{- Add -}

insertPerson person = runIOPool $ insert person

{- Update -}

uCScore pid       = runIOPool $ update (toKeyP pid) [PersonChallengeScore +=. 3]
uScore pid score  = uDB (toKeyP pid) PersonScore score
uName pid nName   = uDB (toKeyP pid) PersonName nName

suId pid = uDB pid PersonDId (gInt $ keyOutLB pid)

{- Get -}

getByID      (pid :: Int) = sList (PersonId  ==. toKeyP pid)

getTop          = runIOPool (selectList [] [Desc PersonScore, LimitTo 10])
getTopChallenge = runIOPool (selectList [] [Desc PersonChallengeScore, LimitTo 10])

getPlayerRank (score :: Int) = runIOPool (count [PersonScore >. score])
getPlayerRankC (score :: Int) = runIOPool (count [PersonChallengeScore >. score])

getPlayerRandom w = runIOPool (rawSql (getWithout w) [])

{- Tools -}
gInt (SqlBackendKey k) = fromIntegral k

keyOutLB (PersonKey key) = key

toKeyP key = (PersonKey $ fromIntegral key)

extract [] = []
extract (e:ent) = eV : extract ent
	where
		eV = entityVal e

getWithout w = 
	append (append "select ?? from Person where id!=" (pack $ show w)) " order by random() limit 1"