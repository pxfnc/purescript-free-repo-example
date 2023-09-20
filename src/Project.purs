module Project
  ( Project
  , ProjectErrors
  , ProjectId(..)
  , unwrapProject
  , validateProject
  , validateTitle
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.UUID.Random (UUIDv4)
import Data.UUID.Random as UUIDv4
import Data.Validation.Semigroup (V, invalid, isValid)
import Foreign (ForeignError(..), fail)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (chooseInt, suchThat)

{-| プロジェクトを表す型 -}
newtype Project = UnsafeMakeProject
  { id :: ProjectId
  , title :: String
  , goalPrice :: Int
  }

instance showProject :: Show Project where
  show (UnsafeMakeProject p) = show p

instance arbitraryProject :: Arbitrary Project where
  arbitrary = UnsafeMakeProject <$> suchThat r (validateProject >>> isValid)
    where
    r = { id: _, title: _, goalPrice: _ }
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

unwrapProject
  :: Project
  -> { id :: ProjectId
     , title :: String
     , goalPrice :: Int
     }
unwrapProject (UnsafeMakeProject p) = p

{-| プロジェクトIDを表す型 -}
newtype ProjectId = ProjectId UUIDv4

instance arbitraryProjectId :: Arbitrary ProjectId where
  arbitrary = ProjectId <$> UUIDv4.make' chooseInt

derive newtype instance eqProjectId :: Eq ProjectId
derive newtype instance ordProjectId :: Ord ProjectId
derive newtype instance showProjectId :: Show ProjectId

instance readForeignProjectId :: ReadForeign ProjectId where
  readImpl ipt = do
    str <- readImpl ipt
    case UUIDv4.fromString str of
      Just uuid -> pure $ ProjectId uuid
      Nothing -> fail $ ForeignError ("invalid ProjectId: " <> str)

instance writeForeignProjectId :: WriteForeign ProjectId where
  writeImpl (ProjectId id) = writeImpl $ UUIDv4.toString id

type ProjectErrors = Array String

{-| 入力のレコードからProjectを作成する。プロジェクトデータが作成されるか、0個以上のエラーが出る -}
validateProject
  :: { id :: ProjectId
     , title :: String
     , goalPrice :: Int
     }
  -> V ProjectErrors Project
validateProject r = map UnsafeMakeProject $
  { id: r.id, title: _, goalPrice: _ }
    <$> validateTitle r.title
    <*> validateGoalPrice r.goalPrice

-- | タイトルのバリデーション
validateTitle :: String -> V ProjectErrors String
validateTitle ipt = case String.length ipt of
  count
    | count <= 0 -> invalid $ pure "title must be not empty"
    | count >= 33 -> invalid $ pure "title must be less than 32 characters"
    | otherwise -> pure ipt

-- | 目標金額のバリデーション
validateGoalPrice :: Int -> V ProjectErrors Int
validateGoalPrice ipt
  | ipt <= 0 = invalid $ pure "goal price must be greater than 0"
  | ipt > 1_000_000_000 = invalid $ pure "goal price must be less than 1,000,000,000"
  | otherwise = pure ipt

