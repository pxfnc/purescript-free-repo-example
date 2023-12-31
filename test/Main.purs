module Test.Main where

import Prelude

import Effect (Effect)
import Test.ProjectSpec as ProjectSpec
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  ProjectSpec.testSuite

