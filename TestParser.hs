module ParserTest where

import Parser (parseLines, splitLine)
import Test.HUnit

testSplitLineVanilla = TestCase $ assertEqual
  "Destructure a string to a String and Int tuple"
  ("And a lazy one", 45)
  (splitLine "And a lazy one 45min")

testPairOfLines = TestCase $ assertEqual
  "Every line should contain name, time pairs"
  [("Create a lightning fast app", 5), ("And a lazy one", 45)]
  (parseLines ["Create a lightning fast app lightning","And a lazy one 45min"])

tests = TestList [TestLabel "testSplitLineVanilla" testSplitLineVanilla,
                  TestLabel "testPairOfLines" testPairOfLines]

main = runTestTT $ tests
