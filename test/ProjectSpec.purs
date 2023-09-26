module Test.ProjectSpec (testSuite) where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.String as String
import Project (Project, mkProject, unwrapProject)
import Test.QuickCheck (Result(..), (<?>), (===))
import Test.Unit (TestSuite, describe, it)
import Test.Unit.QuickCheck (quickCheck)

testSuite :: TestSuite
testSuite =
  describe "Project" do

    it "32文字より多いタイトルの場合、バリデーションエラーになる" do

      quickCheck \(project :: Project) (title :: String) ->
        let
          record = (unwrapProject project) { title = title }
          newProject = mkProject record
        in
          (String.length title > 32) == isLeft newProject
            <?> "タイトルの長さとバリデーション結果が一致しません。" <> "タイトル: " <> show title <> ", バリデーション結果" <> show newProject

    it "Projectは必ずバリデーションが通るようなものが作成される" do
      quickCheck \(project :: Project) ->
        mkProject (unwrapProject project) === Right project

    it "バリデーションが通る場合、入力値は加工されない" do
      quickCheck $ \record ->
        case mkProject record of
          Right project -> unwrapProject project === record
          Left _ -> Success

