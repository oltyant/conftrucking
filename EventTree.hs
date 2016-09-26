module EventTree where

data ScheduledEventTree  = EmptyNode | Node ScheduledEvent (Tree ScheduledEvent) (Tree ScheduledEvent) deriving (Show, Eq, Read)
type Event = (String, Int)
type ScheduledEvent = (String, Int, Session)

data Color = White | Black deriving (Show, Eq, Read)

class NodeColor a where
  colorize :: a -> Color

instance NodeColor ScheduledEventTree where
  colorize EmptyNode                     = White
  colorize Node (_, _, NotScheduled) _ _ = White
  colorize Node (_, _, (_ (min, max) length)) l r
    | length > max                               = Black
    | colorize l == Black && colorize r == Black = Black
    | otherwise                                  = White

singleton :: (ScheduledEvent a) => a -> Tree a
singleton a = Node a EmptyNode EmptyNode

treeInsert :: (Session s, Event e, ScheduledEvent se, ScheduledEventTree t) => s -> (s -> e -> se) -> t -> e -> t
treeInsert sess f EmptyNode a = singleton $ f sess a
treeInsert sess f tree@(Node x l r) a
  | eventOfx == a                     = tree
  | colorize tree == Black            = tree
  | l == EmptyNode && r == EmptyNode  = Node x (Node (f sess a) EmptyNode EmptyNode) (Node (f NotScheduled a) EmptyNode EmptyNode)
  | colorize l == White               = Node x (treeInsert sess f l a) r
  | colorize r == White               = Node x l (treeInsert sess r a)
  | otherwise                         = tree
    where
      eventOfx = (fst x, scnd x)
