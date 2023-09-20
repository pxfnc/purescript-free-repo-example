module App where

import Prelude

import Data.Either (Either(..))
import Data.Validation.Semigroup (V(..))
import Project (Project, ProjectErrors, validateProject)
import ProjectRepository (ProjectRepositoryDSL, mkProjectId, saveProject)

{-| プロジェクトを作成するユースケース -}
createProject
  :: { title :: String, goalPrice :: Int }
  -> ProjectRepositoryDSL (Either ProjectErrors Project)
createProject { title, goalPrice } = do

  -- idの採番
  id <- mkProjectId

  -- mkProjectでバリデーション実行 
  case validateProject { id, title, goalPrice } of

    -- 成功したなら保存し、validProjectを返却
    V (Right validProject) -> saveProject validProject $> Right validProject

    -- バリデーションエラーがある場合、保存をせずにエラーを返却
    V err -> pure err
