module TestEventTree where

import EventTree
import Test.HUnit

testtree1 = ((Node [("And a lazy one", 45)] 45
    (Node (("1", 15):[("And a lazy one", 45)]) 60 EmptyNode EmptyNode)
    (Node [("And a lazy one", 45)] 45 EmptyNode EmptyNode)), [[]])

testtree2 = ((Node [("And a lazy one", 45)] 45
    (Node (("1", 15):[("And a lazy one", 45)]) 60
     (Node (("2", 120):("1",15):[("And a lazy one", 45)]) 180 EmptyNode EmptyNode)
     (Node (("1", 15):[("And a lazy one", 45)]) 60 EmptyNode EmptyNode))
    (Node [("And a lazy one", 45)] 45
     (Node (("2", 120):[("And a lazy one", 45)]) 165 EmptyNode EmptyNode)
     (Node [("And a lazy one", 45)] 45 EmptyNode EmptyNode))), [(("2", 120):("1",15):[("And a lazy one", 45)]),[]])

testSingleton = TestCase $ assertEqual
  "Creating a singleton EventTree"
  (Node [("And a lazy one", 45)] 45 EmptyNode EmptyNode)
  (singleton ("And a lazy one", 45))

testTreeInsert = TestCase $ assertEqual
  "An insert into EventTree have to create a new level"
  testtree1
  (treeInsert (180, 180) (singleton ("And a lazy one", 45)) ("1", 15))

testTreeInsertResult = TestCase $ assertEqual
  "An insert into EventTree has to create a new level"
  testtree2
  (treeInsert (180, 180) (fst testtree1) ("2", 120))

tests = TestList [TestLabel "Creating a singleton EventTree" testSingleton,
                  TestLabel "Insertion into EventTree" testTreeInsert,
                  TestLabel "Insertion into EventTree and get result" testTreeInsertResult]

main = runTestTT $ tests
