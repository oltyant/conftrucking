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

testEvents1 = [("1", 20),("2", 20),("3",40),("4",5),("5", 30), ("6",60),("7",10)]

rootNode = (Node [] 0 EmptyNode EmptyNode)

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

testFindFirst = TestCase $ assertEqual
  "Find the first result in the EventTree"
  [[("3",40),("2",20),("1",20)]]
  (filter (not . null) (snd (findFirst (80,80) rootNode testEvents1)))

testFindAfter5Insert = TestCase $ assertEqual
  "Find the result after 5 insertTree step"
  [[("3",40),("2",20),("1",20)]]
  (filter (not . null) (snd (findNStep (80,80) rootNode testEvents1 5)))

testFindAfter6Insert = TestCase $ assertEqual
  "Find the result after 6 insertTree step"
  [[("3",40),("2",20),("1",20)],[("6",60),("1",20)],[("6",60),("2",20)]]
  (filter (not . null) (snd (findNStep (80,80) rootNode testEvents1 6)))

testFindAll = TestCase $ assertEqual
  "Find all the result (build up the whole tree)"
  [[("3",40),("2",20),("1",20)],[("6",60),("1",20)],[("6",60),("2",20)],[("7",10),("5",30),("2",20),("1",20)],[("7",10),("5",30),("3",40)]]
  (filter (not . null) (snd (findAll (80,80) rootNode testEvents1)))
   
tests = TestList [TestLabel "Creating a singleton EventTree" testSingleton,
                  TestLabel "Insertion into EventTree" testTreeInsert,
                  TestLabel "Insertion into EventTree and get result" testTreeInsertResult,
                  TestLabel "Find the first not empty result" testFindFirst,
                  TestLabel "Find the result after 5 insertion" testFindAfter5Insert,
                  TestLabel "Find the result after 6 insertion" testFindAfter6Insert,
                  TestLabel "Find all the result and build up the whole tree" testFindAll]

main = runTestTT $ tests
