{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model where

import qualified Web.Spock.Simple as SP

import Data.Text
import Data.Aeson
import Control.Monad
import Control.Applicative
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  dId Int
  name Text
  score Int
  challengeScore Int
  deriving Show
|]

{- JSON -}

instance FromJSON Person where
 parseJSON (Object v) =
    Person <$> v .: "dId"
           <*> v .: "name"
           <*> v .: "score"
           <*> v .: "challengeScore"
 parseJSON _ = mzero

instance ToJSON Person where
 toJSON (Person dId name score challengeScore) =
    object [ "dId"            .= dId
           , "name"           .= name
           , "score"          .= score 
           , "challengeScore" .= challengeScore ]

{- Keys-}

k_score      = "nScore" :: Text
k_CScore     = "cScore" :: Text
k_playerID   = "pID" :: Text
k_playerName = "name" :: Text

{- Tools -}

jSucces = SP.json $ ["succes" :: Text]

newPID name = Person (-1) name 0 0

centerT t = wrapT "<center>" "</center>" t
h1T t = wrapT"<h1>" "</h1>" t

wrapT b e t = append (append b t) e

canLogin user pass = do
  (_,u,p) <- SP.getState
  return (u == user && p == pass)

auth action = SP.requireBasicAuth "AUTH" canLogin $ action