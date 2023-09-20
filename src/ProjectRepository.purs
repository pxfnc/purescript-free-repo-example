module ProjectRepository
  ( ProjectRepositoryF(..)
  , ProjectRepositoryDSL
  , getProjectById
  , mkProjectId
  , saveProject
  ) where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Maybe (Maybe)
import Project (Project, ProjectId)

data ProjectRepositoryF r
  = MkProjectId (ProjectId -> r)
  | GetProjectById ProjectId (Maybe Project -> r)
  | SaveProject Project (Unit -> r)

instance functorProjectRepository :: Functor ProjectRepositoryF where
  map f (MkProjectId k) = MkProjectId (f <<< k)
  map f (GetProjectById id k) = GetProjectById id (f <<< k)
  map f (SaveProject p k) = SaveProject p (f <<< k)

type ProjectRepositoryDSL = Free ProjectRepositoryF

mkProjectId :: ProjectRepositoryDSL ProjectId
mkProjectId = liftF (MkProjectId identity)

getProjectById :: ProjectId -> ProjectRepositoryDSL (Maybe Project)
getProjectById id = liftF (GetProjectById id identity)

saveProject :: Project -> ProjectRepositoryDSL Unit
saveProject p = liftF (SaveProject p identity)
