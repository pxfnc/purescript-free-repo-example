module Main
  ( main
  ) where

import Prelude

import App (createProject)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple.Nested ((/\))
import Database.SQLite3 as SQLite3
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log, logShow)
import Project (Project, ProjectErrors, ProjectId, unwrapProject)
import ProjectRepository (ProjectRepositoryDSL, getProjectById, mkProjectId)
import ProjectRepository.OnMemory as OnMemoryRepo
import ProjectRepository.Sqlite3 as Sqlite3Repo
import Random.LCG (mkSeed)

-- | 適当に3つのプロジェクトを作成するアプリ
createThreeProjects :: ProjectRepositoryDSL (Array (Either ProjectErrors Project))
createThreeProjects = sequence
  [ createProject { title: "My first project", goalPrice: 1_000_000 }
  , createProject { title: "second project", goalPrice: 1_000_000 }
  , createProject { title: "third project", goalPrice: 1 }
  ]

getProjectsByIds :: Array ProjectId -> ProjectRepositoryDSL (Array (Maybe Project))
getProjectsByIds = traverse getProjectById

sqliteMain :: Effect Unit
sqliteMain = launchAff_ $ SQLite3.withDatabase ":memory:" $ \db -> do

  log "vvvv  SQLite3  vvvv"

  runReaderT Sqlite3Repo.init db

  logShow =<< Sqlite3Repo.run db do
    mkProjectId >>= getProjectById

  results <- Sqlite3Repo.run db $ createThreeProjects
  logShow results

  case sequence results of
    Right projects -> do
      let ids = map (unwrapProject >>> _.id) projects
      fetchedProjects <- Sqlite3Repo.run db $ getProjectsByIds ids
      logShow fetchedProjects
    Left _ -> pure unit
  log "^^^^  SQLite3  ^^^^"
  log ""

onMemoryMain :: Effect Unit
onMemoryMain = do

  log "vvvv  OnMemory  vvvv"

  let
    db = Map.empty
    st = { newSeed: mkSeed 1, size: 1 }
    results /\ db' /\ st' = OnMemoryRepo.run db st $ createThreeProjects

  logShow results

  case sequence results of
    Right projects -> do
      let
        ids = map (unwrapProject >>> _.id) projects
        fetchedProjects /\ _ = OnMemoryRepo.run db' st' $ getProjectsByIds ids
      logShow fetchedProjects
    Left _ -> pure unit

  log "^^^^  OnMemory  ^^^^"
  log ""

main :: Effect Unit
main = do
  sqliteMain
  onMemoryMain
