import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l <> tag r) l r

indexJ ::
  (Sized b, Monoid b) =>
  Int ->
  JoinList b a ->
  Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i (Single _ _) = Nothing
indexJ i (Append b l r) | getSize (size (tag l)) < i = indexJ i l
indexJ i (Append b l r) = indexJ (i - getSize (size (tag l))) r

dropJ ::
  (Sized b, Monoid b) =>
  Int ->
  JoinList b a ->
  JoinList b a
dropJ i jl | i <= 0 = jl
dropJ i jl | i > getSize (size (tag jl)) = Empty
dropJ i (Append b l r) = dropJ i l +++ dropJ (i - getSize (size (tag l))) r

takeJ ::
  (Sized b, Monoid b) =>
  Int ->
  JoinList b a ->
  JoinList b a
takeJ i jl | i <= 0 = Empty
takeJ i jl | i > getSize (size (tag jl)) = jl
takeJ i (Append b l r) = takeJ i l +++ takeJ (i - getSize (size (tag l))) r
