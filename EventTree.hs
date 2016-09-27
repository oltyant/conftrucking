module EventTree where

data EventTree  = EmptyNode | Node [Event] Int EventTree EventTree deriving (Show, Eq, Read)
type Event      = (String, Int)
type Result     = (EventTree, [[Event]])

singleton :: Event -> EventTree
singleton a = Node (a:[]) (snd a) EmptyNode EmptyNode


treeInsert :: (Int, Int) -> EventTree -> Event -> Result
treeInsert (min, max) EmptyNode e = (etree, xs)
  where
    etree@(Node _ t _ _) = singleton e
    xs                   = if t >= min && t <= max then [e:[]] else [[]]
treeInsert (min, max) tr@(Node es time l r) e
  | time > max                        = (tr, [[]])
  | goal time                         = (tr, [[]])
  | isEmptyLeafs                      = (Node es time newl newr, res)
  | time < max                        = (Node es time (fst lins) (fst rins), (snd lins ++ snd rins))
  | otherwise                         = (tr, [[]])
    where
      goal t           = t >= min && t <= max
      newl             = Node (e:es) (time + (snd e)) EmptyNode EmptyNode
      newr             = Node es time EmptyNode EmptyNode
      res              = if goal $ time + snd e then [(e:es)] else [[]]
      lins             = treeInsert (min, max) l e
      rins             = treeInsert (min, max) r e
      isEmptyLeafs     = l == EmptyNode && r == EmptyNode


findFirst :: (Int, Int) -> EventTree -> [Event] -> Result
findFirst _ t []          = (t, [[]])
findFirst minmax t es     =
  let empty res = null $ snd res in
  let f         = (\acc x -> if empty acc then treeInsert minmax (fst acc) x else acc) in
    foldl f (t, [[]]) es


findAll :: (Int, Int) -> EventTree -> [Event] -> Result
findAll _ t []            = (t, [[]])
findAll minmax t es       =
  let f         = (\acc x -> treeInsert minmax (fst acc) x)
  in foldl f (t, [[]]) es
