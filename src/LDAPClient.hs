{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module LDAPClient where

import           Ldap.Client                   as Ldap
import qualified Ldap.Client.Bind              as Ldap
import qualified Ldap.Client.Search            as Ldap
import           Data.Maybe
import           Data.Ini
import           Data.Either.Combinators
import qualified Data.Text                     as T
import           Data.Text.Read
import           Data.Text.Encoding
import           Control.Exception

data Config = Config
  { host     :: T.Text
  , port     :: Int
  , dn       :: Dn
  , password :: Password
  , groupsDn     :: Dn
  , peopleDn     :: Dn
  , activePeopleDn     :: Dn
  , inactivePeopleDn     :: Dn
  , guestsDn     :: Dn
  } deriving (Show, Eq)

getConfig :: IO Config
getConfig = do
  result <- readIniFile "files/config.ini"
  case result of
    Left  str -> error str
    Right ini -> pure Config
      { host             = get "LDAP_HOST" ini
      , port             = fst . fromRight' . decimal $ get "LDAP_PORT" ini
      , dn               = Ldap.Dn $ get "BIND_DN" ini
      , password         = Ldap.Password $ encodeUtf8 $ get "BIND_PW" ini
      , groupsDn         = Ldap.Dn $ get "DN_GROUPS" ini
      , peopleDn         = Ldap.Dn $ get "DN_PEOPLE" ini
      , activePeopleDn   = Ldap.Dn $ get "DN_PEOPLE_ACTIVE" ini
      , inactivePeopleDn = Ldap.Dn $ get "DN_PEOPLE_INACTIVE" ini
      , guestsDn         = Ldap.Dn $ get "DN_PEOPLE_GUESTS" ini
      }
 where
  get :: T.Text -> Ini -> T.Text
  get x ini = fromRight' $ lookupValue "SERVER" x ini

data LDAPClientException
  = NoSuchUser
  | LDAPException ResponseError
  deriving (Show)
instance Exception LDAPClientException

getUsers :: Ldap -> Dn -> IO (Either ResponseError [SearchEntry])
getUsers ldap dn = Ldap.searchEither ldap
                                     dn
                                     mempty
                                     (Attr "objectClass" := "inetOrgPerson")
                                     [Attr "cn", Attr "uid", Attr "mail"]

getUser :: Ldap -> Dn -> T.Text -> IO (Either LDAPClientException SearchEntry)
getUser ldap dn uid = do
  result <- Ldap.searchEither
    ldap
    dn
    mempty
    (And [Attr "objectClass" := "inetOrgPerson", Attr "uid" := encodeUtf8 uid])
    [Attr "cn", Attr "uid", Attr "mail"]
  case result of
    Left  err -> pure . Left $ LDAPException err
    Right res -> pure $ if null res then Left NoSuchUser else Right $ head res
