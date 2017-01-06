module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Foreign (Foreign, ForeignError, parseJSON)
import Data.Foreign.Class (class IsForeign, class AsForeign, read, write)
import Data.Foreign.Generic (defaultOptions, readGeneric, readJSONGeneric, toForeignGeneric, toJSONGeneric)
import Data.Foreign.Generic.Types (Options, SumEncoding(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList)




newtype UserId = UserId Int
newtype Email = Email String


derive instance eqUserId :: Eq UserId
derive instance eqEmail :: Eq Email

derive newtype instance showUserId :: Show UserId
derive newtype instance showEmail :: Show Email


derive newtype instance isForeignUserId :: IsForeign UserId
derive newtype instance isForeignEmail :: IsForeign Email

derive newtype instance asForeignUserId :: AsForeign UserId
derive newtype instance asForeignEmail :: AsForeign Email

data Subscription = Tech | Sports | Music

derive instance eqSubscription :: Eq Subscription
derive instance genericSubscription :: Generic Subscription _

instance showSubscription :: Show Subscription where
  show = genericShow

instance isForeignSubscription :: IsForeign Subscription where
  read x = readGeneric defaultOptions x

instance asForeignSubscription :: AsForeign Subscription where
  write x = toForeignGeneric defaultOptions x

data User = Anonymous { id :: UserId, name :: String } |
            Registered { id :: UserId, email :: Email, name :: String, subscription :: Subscription}


derive instance eqUser :: Eq User
derive instance genericUser :: Generic User _

instance showUser :: Show User where
  show = genericShow


instance isForeignUser :: IsForeign User where
  read x = readGeneric defaultOptions x

instance asForeignUser :: AsForeign User where
  write x = toForeignGeneric defaultOptions x


testEquality :: User -> User -> Boolean
testEquality u1 u2 = u1 == u2

testEqualityForeign :: Foreign -> Foreign -> Either (NonEmptyList ForeignError) (Boolean)
testEqualityForeign fu1 fu2 = runExcept do
  u1 <- read fu1
  u2 <- read fu2
  pure $ testEquality u1 u2


anon :: User
anon = Anonymous { id : (UserId 10), name: "Anon" }

registered :: User
registered = Registered { id: (UserId 20), name: "Registered", email: (Email "registered@com.com") , subscription : Tech}


-- With custom options, sum types will be encoded as { constructor: "Anonymous", data: {  name: "Anon", id: 10 } }
-- instead of the default { tag: "Anonymous", contents: {  name: "Anon", id: 10 } }
customOptions :: Options
customOptions = { sumEncoding :
  TaggedObject
    { tagFieldName: "constructor"
    , contentsFieldName: "data"
    }
    -- not sure what this does, but thats the default
  , unwrapSingleConstructors: false
    -- not sure what this does, but thats the default
  , unwrapSingleArguments: true
}


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  -- Test a round trip
  -- output: (Right true)
  logShow $ testEqualityForeign (write anon) (write anon)

  -- Test a round trip
  -- output: (Right false)
  logShow $ testEqualityForeign (write anon) (write registered)

  -- output: "{\"contents\":{\"name\":\"Anon\",\"id\":10},\"tag\":\"Anonymous\"}"
  logShow $ toJSONGeneric defaultOptions anon

  -- output: "{\"contents\":{\"name\":\"Registered\",\"id\":20,\"email\":\"registered@com.com\"},\"tag\":\"Registered\"}"
  logShow $ toJSONGeneric defaultOptions registered

  -- output: (Right (Registered { email: "registered@com.com", id: 20, name: "Registered" }))
  logShow $ runExcept do
    value <- parseJSON "{\"contents\":{\"subscription\":{\"tag\":\"Tech\"},\"name\":\"Registered\",\"id\":20,\"email\":\"registered@com.com\"},\"tag\":\"Registered\"}"
    u :: User <- read value
    pure u


  let json = "{\"contents\":{\"subscription\":{\"tag\":\"Tech\"},\"name\":\"Registered\",\"id\":20,\"email\":\"registered@com.com\"},\"tag\":\"Registered\"}"
  -- output: (Right (Registered { email: "registered@com.com", id: 20, name: "Registered" }))
  logShow $ runExcept do
    u :: User <- readJSONGeneric defaultOptions json
    pure u

  let jsonCustom = "{\"data\":{\"subscription\":{\"tag\":\"Tech\"},\"name\":\"Registered\",\"id\":20,\"email\":\"registered@com.com\"},\"constructor\":\"Registered\"}"
  -- output: (Right (Registered { email: "registered@com.com", id: 20, name: "Registered" }))
  logShow $ runExcept do
    u :: User <- readJSONGeneric customOptions jsonCustom
    pure u

  -- output: (Left (NonEmptyList (NonEmpty (ErrorAtProperty "data" (TypeMismatch "String" "Function")) ((ErrorAtProperty "data" (TypeMismatch "String" "Function")) : Nil))))
  logShow $ runExcept do
    u :: User <- readJSONGeneric customOptions json
    pure u

