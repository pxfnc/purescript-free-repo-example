module ProjectRepository.Sqlite3
  ( init
  , run
  ) where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Free (foldFree)
import Control.Monad.Reader (class MonadReader, ReaderT, runReaderT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Traversable (traverse)
import Data.UUID.Random as UUIDv4
import Data.Validation.Semigroup (toEither)
import Database.SQLite3 as Sqlite3
import Effect.Aff (Aff, Error, error)
import Effect.Aff.Class (class MonadAff)
import Foreign (ForeignError, renderForeignError)
import Project (ProjectErrors, ProjectId(..), unwrapProject, validateProject)
import ProjectRepository (ProjectRepositoryDSL, ProjectRepositoryF(..))
import Simple.JSON (read, write)

init :: forall m. MonadReader Sqlite3.Database m => MonadAff m => m Unit
init =
  Sqlite3.run
    """
  CREATE TABLE IF NOT EXISTS projects (id TEXT PRIMARY KEY, title TEXT NOT NULL, goal_price INT NOT NULL);
  """
    []

run :: forall a. Sqlite3.Database -> ProjectRepositoryDSL a -> Aff a
run db = flip runReaderT db <<< interpret

interpret :: forall a. ProjectRepositoryDSL a -> ReaderT Sqlite3.Database Aff a
interpret = foldFree case _ of

  GetProjectById id next -> do
    result <- Sqlite3.get "SELECT id, title, goal_price as goalPrice from projects where id = ?" [ write id ]
    maybeRecord <- liftEither <<< lmap fromForeignError $ read result
    maybeProject <- liftEither <<< lmap fromProjectErrors <<< toEither <<< traverse validateProject $ maybeRecord
    pure $ next maybeProject

  SaveProject project next -> do
    let { id, title, goalPrice } = unwrapProject project
    Sqlite3.run "INSERT INTO projects (id, title, goal_price) VALUES (?, ?, ?)" [ write id, write title, write goalPrice ] <#> next

  MkProjectId next ->
    map ProjectId UUIDv4.make <#> next

fromForeignError :: NonEmptyList ForeignError -> Error
fromForeignError = error <<< NEL.intercalate "\n" <<< map renderForeignError

fromProjectErrors :: ProjectErrors -> Error
fromProjectErrors = error <<< Array.intercalate "\n"