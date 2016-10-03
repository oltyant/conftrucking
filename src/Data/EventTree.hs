module EventTree where

import qualified Data.List as L 

data EventTree  = EmptyNode | Node [Event] Int EventTree EventTree deriving (Show, Eq, Read)
type Event      = (String, Int)
type Result     = (EventTree, [[Event]])
type Range      = (Int, Int)

singleton :: Event -> EventTree
singleton a = Node [a] (snd a) EmptyNode EmptyNode


treeInsert :: Range -> EventTree -> Event -> Result
treeInsert (min, max) EmptyNode e = (etree, xs)
  where
    etree@(Node _ t _ _) = singleton e
    xs                   = if t >= min && t <= max then [[e]] else [[]]
treeInsert (min, max) tr@(Node es time l r) e
  | l `contains` e                    = (tr, [[]])
  | time > max                        = (tr, [[]])
  | intime time                       = (tr, [[]])
  | isEmptyLeafs                      = (Node es time newl newr, res)
  | time < max                        = (Node es time (fst lins) (fst rins), (snd lins ++ snd rins))
  | otherwise                         = (tr, [[]])
    where
      contains ltree e = case ltree of
        (Node xs _ _ _) -> head xs == e
        _               -> False
      intime t         = t >= min && t <= max
      newl             = Node (e:es) (time + snd e) EmptyNode EmptyNode
      newr             = Node es time EmptyNode EmptyNode
      res              = if intime $ time + snd e then [e:es] else [[]]
      lins             = treeInsert (min, max) l e
      rins             = treeInsert (min, max) r e
      isEmptyLeafs     = l == EmptyNode && r == EmptyNode


findFirst :: Range -> EventTree -> [Event] -> Result
findFirst _ t []          = (t, [[]])
findFirst minmax t es     =
  let empty res = null $ head $ snd res in
  let f acc x   = if empty acc then treeInsert minmax (fst acc) x else acc in
    foldl f (t, [[]]) es


findNStep :: Range -> EventTree -> [Event] -> Int -> Result
findNStep _ t [] _          = (t, [[]])
findNStep _ t _ 0           = (t, [[]])
findNStep minmax t es n     = findAll minmax t $ take n es

  
findAll :: Range -> EventTree -> [Event] -> Result
findAll _ t []            = (t, [[]])
findAll minmax t es       = foldl f (t, [[]]) es
  where
        f acc x = let (newtree, res) = treeInsert minmax (fst acc) x in
                (newtree, snd acc ++ res)
      
