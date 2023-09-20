module Database.SQLite3
  ( Database
  , Sql
  , FilePath
  , all
  , close
  , database
  , get
  , run
  , withDatabase
  ) where

import Prelude

import Control.Monad.Reader.Class (class MonadReader, ask)
import Effect.Aff (Aff, bracket)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign)

foreign import data Database :: Type

foreign import _database :: String -> EffectFnAff Database

foreign import _close :: Database -> EffectFnAff Unit
foreign import _run :: Sql -> Array Foreign -> Database -> EffectFnAff Unit
foreign import _get :: Sql -> Array Foreign -> Database -> EffectFnAff Foreign
foreign import _all :: Sql -> Array Foreign -> Database -> EffectFnAff (Array Foreign)

type Sql = String
type FilePath = String

database :: FilePath -> Aff Database
database filepath = fromEffectFnAff (_database filepath)

close :: Database -> Aff Unit
close db = fromEffectFnAff (_close db)

lift' :: forall m a. MonadReader Database m => MonadAff m => (Database -> EffectFnAff a) -> m a
lift' toEff = ask >>= toEff >>> fromEffectFnAff >>> liftAff

run :: forall m. MonadReader Database m => MonadAff m => Sql -> Array Foreign -> m Unit
run sql params = lift' $ _run sql params

get :: forall m. MonadReader Database m => MonadAff m => Sql -> Array Foreign -> m Foreign
get sql params = lift' $ _get sql params

all :: forall m. MonadReader Database m => MonadAff m => Sql -> Array Foreign -> m (Array Foreign)
all sql params = lift' $ _all sql params

withDatabase :: forall a. FilePath -> (Database -> Aff a) -> Aff a
withDatabase filepath = bracket (database filepath) close