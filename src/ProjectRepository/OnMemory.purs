module ProjectRepository.OnMemory (run, runDefault, interpret) where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Free (foldFree)
import Control.Monad.State (StateT, gets, modify_, runStateT)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Project (Project, ProjectId, unwrapProject)
import ProjectRepository (ProjectRepositoryDSL, ProjectRepositoryF(..))
import Random.LCG (mkSeed)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, GenState, runGen)

{-| 初期のdbと、乱数生成機の状態を当たることで純粋にプログラムを実行する
入力に対して必ず同じ出力を返すので、ビジネスロジックのエッジケースを検証するのに便利
-}
run
  :: forall a
   . Map ProjectId Project
  -> GenState
  -> ProjectRepositoryDSL a
  -> a /\ Map ProjectId Project /\ GenState
run db st program = result /\ db' /\ st'
  where
  gen = runStateT (interpret program) db
  (result /\ db') /\ st' = runGen gen st

runDefault :: forall a. ProjectRepositoryDSL a -> a
runDefault = fst <<< run Map.empty { newSeed: mkSeed 1, size: 1 }

{-| `Free ProjectRepositoryF a` データ型を `StateT (Map ProjectId Project Gen` モナドとして解釈する

実装方法としては、`foldFree`関数に1ステップ毎の意味`ProjectRepositoryF ~> StateT (Map ProjectId Project) Gen`の対応を与えることで、
全体 `Free ProjectRepositoryF a`の意味`StateT (Map ProjectId Project) Gen`を得る

-}
interpret :: ProjectRepositoryDSL ~> StateT (Map ProjectId Project) Gen
interpret = foldFree case _ of
  GetProjectById id next ->
    gets (Map.lookup id) <#> next
  SaveProject project next ->
    modify_ (Map.insert (unwrapProject project # _.id) project) <#> next
  MkProjectId next ->
    lift arbitrary <#> next
