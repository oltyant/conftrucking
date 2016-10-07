module TestBruteForce where

import BruteForce
import Test.HUnit

testEvents = [("Event1", 30), ("Event2", 10), ("Event3", 20), ("Event4", 10)]

testFindAll = TestCase $ assertEqual
  "Should give proper Result array based on the input events"
  [[("Event1", 30), ("Event2", 10)], [("Event1", 30), ("Event4", 10)],[("Event2", 10), ("Event3", 20), ("Event4", 10)]]
  (findAll (40, 40) testEvents)

tests = TestList [TestLabel "Computing a single result based on the given test events" testFindAll]

main = runTestTT $ tests
