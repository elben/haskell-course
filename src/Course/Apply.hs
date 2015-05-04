{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Apply where

import Course.Core
import Course.Functor
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P


-- | All instances of the `Apply` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associative composition
--   `∀a b c. ((.) <$> a <*> b <*> c) ≅ (a <*> (b <*> c))`
class Functor f => Apply f where
  -- Pronounced apply.
  (<*>) ::
    f (a -> b)
    -> f a
    -> f b

infixl 4 <*>

-- | Implement @Apply@ instance for @Id@.
--
-- >>> Id (+10) <*> Id 8
-- Id 18
instance Apply Id where
  (<*>) ::
    Id (a -> b)
    -> Id a
    -> Id b
  (<*>) (Id f) a = mapId f a
  -- Equivalent:
  -- (<*>) (Id f) (Id a) = Id $ f a
  --
  -- Equivalent, but the interesting way of using fmap and bind instead of
  -- pattern matching on Id type. This means that if a data type implements Bind
  -- and Functor, it can get the 'default' implementation of Apply for free,
  -- given the laws hold. But in Prelude, there is no default impl, so you have
  -- to specify still:
  --
  -- (<*>) f a = bindId (\f' -> (mapId f' a)) f

-- | Implement @Apply@ instance for @List@.
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
instance Apply List where
  (<*>) ::
    List (a -> b)
    -> List a
    -> List b
  (f :. fs) <*> as = (map f as) ++ (fs <*> as)
  Nil <*> _ = Nil

-- | Implement @Apply@ instance for @Optional@.
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
instance Apply Optional where
  (<*>) ::
    Optional (a -> b)
    -> Optional a
    -> Optional b
  f <*> a = bindOptional (\f' -> mapOptional f' a) f

  -- Equivalent implementation:
  -- (Full f) <*> (Full a) = Full $ f a
  -- _ <*> _ = Empty

-- | Implement @Apply@ instance for reader.
--
-- >>> ((+) <*> (+10)) 3
-- 16
--
-- >>> ((+) <*> (+5)) 3
-- 11
--
-- >>> ((+) <*> (+5)) 1
-- 7
--
-- >>> ((*) <*> (+10)) 3
-- 39
--
-- >>> ((*) <*> (+2)) 3
-- 15
--
-- This is an interesting instance. So the `t` that is finally given by the user
-- is used twice, once on the left side, once on the right side.
instance Apply ((->) t) where
  (<*>) ::
    ((->) t (a -> b))
    -> ((->) t a)
    -> ((->) t b)
  f <*> g = \t -> (f t) (g t)

-- | Apply a binary function in the environment.
--
-- >>> lift2 (+) (Id 7) (Id 8)
-- Id 15
--
-- >>> lift2 (+) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil)
-- [5,6,6,7,7,8]
--
-- >>> lift2 (+) (Full 7) (Full 8)
-- Full 15
--
-- >>> lift2 (+) (Full 7) Empty
-- Empty
--
-- >>> lift2 (+) Empty (Full 8)
-- Empty
--
-- >>> lift2 (+) length sum (listh [4,5,6])
-- 18
--
-- Note in the last example, `lift2 (+) length sum` returns a function, which
-- then is given the list [4,5,6]. This is very cool. Uses the (-> t (a -> b))
-- instance of Apply twice, I think.
--
-- length :: List a -> Int
-- sum    :: List Int -> Int
-- (+)    :: Num a => a -> a -> a
--
-- `(+) <$> length` results in a function like:
-- (\t -> (+) length t) :: Num a => a -> (a -> a)
--
-- Then, `(\t -> (+) length t) <*> sum` returns a function like:
--
-- (\t' -> ((\t -> (+) length t) t') (sum t'))
--
-- That is, find the sum of the lsit, pass this answer to the function that is
-- waiting for the list so that it can calculate the length, then add those two
-- numbers together.
--
lift2 ::
  Apply f =>
  (a -> b -> c)
  -> f a
  -> f b
  -> f c
-- Note: (<$>) :: (a -> z) -> f a -> f z
--    =  (<$>) :: (a -> (b -> c)) -> f a -> f (b -> c)
lift2 f a b = f <$> a <*> b

-- | Apply a ternary function in the environment.
--
-- >>> lift3 (\a b c -> a + b + c) (Id 7) (Id 8) (Id 9)
-- Id 24
--
-- >>> lift3 (\a b c -> a + b + c) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil)
-- [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) (Full 9)
-- Full 24
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) Empty
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty (Full 8) (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty Empty (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) length sum product (listh [4,5,6])
-- 138
lift3 ::
  Apply f =>
  (a -> b -> c -> d)
  -> f a
  -> f b
  -> f c
  -> f d
lift3 f a b c = (lift2 f a b) <*> c

-- | Apply a quaternary function in the environment.
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Id 7) (Id 8) (Id 9) (Id 10)
-- Id 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil) (9 :. 10 :. Nil)
-- [20,21,21,22,22,23,21,22,22,23,23,24,21,22,22,23,23,24,22,23,23,24,24,25,22,23,23,24,24,25,23,24,24,25,25,26]
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) (Full 9) (Full 10)
-- Full 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) Empty  (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty (Full 8) (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty Empty (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) length sum product (sum . filter even) (listh [4,5,6])
-- 148
lift4 ::
  Apply f =>
  (a -> b -> c -> d -> e)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f e
lift4 f a b c d = (lift3 f a b c) <*> d

-- | Sequence, discarding the value of the first argument.
-- Pronounced, right apply.
--
-- >>> [1,2,3] *> [4,5,6]
-- [4,5,6,4,5,6,4,5,6]
--
-- >>> [1,2] *> [4,5,6]
-- [4,5,6,4,5,6]
--
-- >>> [1,2,3] *> [4,5]
-- [4,5,4,5,4,5]
--
-- >>> Full 7 *> Full 8
-- Full 8
--
-- prop> [a,b,c] *> [x,y,z] == [x,y,z,x,y,z,x,y,z]
--
-- prop> Full x *> Full y == Full y
(*>) ::
  Apply f =>
  f a
  -> f b
  -> f b
(*>) = lift2 (\_ -> id)
-- Equivalent implementation:
-- (*>) a b = (\_ -> id) <$> a <*> b
--
-- The above decomposes like this:
-- (<$>) ::   (a -> b) -> f a -> f b
-- Where 'b' is the type of 'id':
-- (<$>) ::   (a -> (b -> b)) -> f a -> f (b -> b)
-- Thus, <*> becomes the type:
-- (<*>) :: f (b -> b) -> f a -> f b
--
-- So when you do:
--
-- [1,2,3] *> [4,5]
--
-- The first part, `(\_ -> id) <$> List [1,2,3]`, gets List's implementation of
-- <$> (which is `map`). This becomes List [id,id,id] :: List (b -> b).
--
-- Then, we have: List [id,id,id] <*> List [4,5]. This uses the List
-- implementation of <*>, where it does the cartesian product, and we get
-- [4,5,4,5,4,5].
--
--
-- An intuitive explanation is that we lift the function (\_ -> id) (a function
-- that returns the id function) into a's computation context (e.g. List Int).
-- We must do this lift of the value-ignoring function `\_ -> id` so that the
-- left-hand argument is ignored WHILE still operating the apply of the
-- *structure* of the left side (e.g. list of 3 things) with the right (e.g.
-- list of 2 things), producing list of 3*2=6 things.

-- | Sequence, discarding the value of the second argument.
-- Pronounced, left apply.
--
-- >>> [1,2,3] <* [4,5,6]
-- [1,1,1,2,2,2,3,3,3]
--
-- >>> [1,2] <* [4,5,6]
-- [1,1,1,2,2,2]
--
-- >>> [1,2,3] <* [4,5]
-- [1,1,2,2,3,3]
--
-- >>> Full 7 <* Full 8
-- Full 7
--
-- prop> [x,y,z] <* [a,b,c] == [x,x,x,y,y,y,z,z,z]
--
-- prop> Full x <* Full y == Full x
(<*) ::
  Apply f =>
  f b
  -> f a
  -> f b
(<*) b a = lift2 const b a
-- Equivalent:
-- (<*) b a = const             <$> b <*> a
--          = (\b' -> const b') <$> b <*> a
--
-- In the complimentary right apply (*>), we use (\_ -> id) so that the value of
-- the right-hand side would be used. In this case, however, we use 'const' so
-- that the value of the left-hand side would be used as we apply through the
-- right-hand side.

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Apply IO where
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

instance Apply [] where
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

instance Apply P.Maybe where
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

(>>) ::
  Apply f =>
  f a
  -> f b
  -> f b
(>>) =
  (*>)
