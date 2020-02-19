{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module RestAPI
  ( restAPI
  )
where

import           Data.Aeson
import           GHC.Generics
import           GHC.TypeLits
import           Servant

-- API type definition
type UserAPI = "guests" :> Get '[JSON] User

newtype User = User
    { name :: String
    } deriving (Eq, Show, Generic)
instance ToJSON User

-- API implementation
server :: Server UserAPI
server = guestsEndpoint

guestsEndpoint :: Handler User
guestsEndpoint = return (User { name = "Test" })

userAPI :: Proxy UserAPI
userAPI = Proxy

restAPI :: Application
restAPI = serve userAPI server
