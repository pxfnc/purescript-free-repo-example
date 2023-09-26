module App where

import Prelude

import Data.Either (Either(..))
import Project (Project, ProjectErrors, mkProject)
import ProjectRepository (ProjectRepositoryDSL, mkProjectId, saveProject)

{-| プロジェクトを作成するユースケース -}
createProject
  :: { title :: String, goalPrice :: Int }
  -> ProjectRepositoryDSL (Either ProjectErrors Project)
createProject { title, goalPrice } = do

  -- idの採番
  id <- mkProjectId

  -- mkProjectでプロジェクトを作成
  case mkProject { id, title, goalPrice } of

    -- 成功したなら保存し、validProjectを返却
    Right validProject -> saveProject validProject $> Right validProject

    -- バリデーションエラーがある場合、保存をせずにエラーを返却
    err -> pure err
