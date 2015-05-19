{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Data.Char
import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to a tuple:
-- (a produced value `a`, and a resulting state `s`).
--
-- The State type has one field (note newtype only allows one field), which is
-- the function s -> (a, s).
--
-- So the type var 's' here is the actual state value (e.g. "success"). Not to
-- be confused with the *field* in the state, which is the function s -> (a, s).
-- That is, when we pattern match in a function argument like (State sf), sf is
-- the function. But the type name "State s" means it's a State intance with
-- the function, and the function contains the state value whose type is 's'.
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- Me playing around:
-- newtype MyState s a = MyState (s -> (a, s))
-- runMyState :: (MyState s a) -> s -> (a, s)
-- runMyState (MyState s) s' = s s'

executeState :: (State s a) -> s -> (a, s)
executeState (State sf) s = sf s
-- executeState (pure 2) 0

-- | Implement the `Functor` instance for `State s`.
-- >>> runState ((+1) <$> pure 0) 0
-- (1,0)
--
-- >>> runState ((+1) <$> State (\s -> (3, s P.++ ["added 3"]))) []
-- (4,["added 3"])
--
-- Analyzing the above example,
--
-- pure 0 becomes:
--   State (\s -> (0,s))
--
-- Then:
--   (+1) <$> pure 0
--   (+1) <$> State (\s -> (0,s))
--   State (\s -> (0+1,s))
--   State (\s -> (1,s))
--
-- Running the computation by passing in a state value of 0:
--   runState (State (\s -> (1,s))) 0
--   = (1,0)
--
instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  (<$>) f (State sf) = State (\s -> let (a, s') = sf s in (f a, s'))

-- | Implement the `Apply` instance for `State s`.
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
instance Apply (State s) where
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b
  (<*>) (State sfab) (State sf) = State (\s -> let (fab, s') = sfab s
                                                   (a, s'')  = sf s'
                                               in (fab a, s''))

-- | Implement the `Applicative` instance for `State s`.
-- >>> runState (pure 2) 0
-- (2,0)
instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure a = State (\s -> (a,s))

-- | Implement the `Bind` instance for `State s`.
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> runState ((\a -> State (\s -> (a+1, s P.++ ["second one"]))) =<< State (\s -> (0, s P.++ ["first one"]))) []
-- (1,["first one","second one"])
instance Bind (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  -- f  :: a -> State s b
  -- fs :: s -> (a, s)
  (=<<) f (State fs) = State (\s -> let (a, s') = fs s
                                        State fs' = f a in fs' s')

instance Monad (State s) where

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec (State sf) s = snd $ sf s

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval (State sf) s = fst $ sf s

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get = State (\s -> (s, s))

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put s = State (\_ -> ((), s))

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
--
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil = pure Empty
findM fb (a :. as) = let r = fb a in r >>= (\b -> if b then pure (Full a) else findM fb as)

-- In this example (from comment above), we first `get` a blank State (function)
-- where the value and state are the same. When we use the blank state, we pass
-- in 0 as the first state value. This is used in the `put (1+s)` expression to
-- increment the count every time the predicate function is run. Note that `put`
-- sets the value as (), since we don't care about the value right now.
--
-- We do, however, have to check each char against 'c', which is what we're
-- looking for. `x` is bound by let as the first arg of p, so `x` is a Char . So
-- once the state is incremented by `put`, we bind (=<<) that into a 'pure'
-- State, whose value is a Boolean. By binding it, we extract the state from the
-- `put` expression and bind it with the value from the predicate check. We use
-- `const` since it needs to be a function. Since the value in the `put`
-- expression is (), we can throw it away (via const), since we will fill up the
-- value (a Boolean) via binding with `pure`.
--
-- In other words, we set a continuation (which is executed by runState passing
-- in the init state value of 0) where once the state init value is passed, we:
--
-- - Use `get` to get an init State (0, 0)
-- - Increment the state for every time the predicate is called: `put (1+s)`
-- - Check the value of each character `x`, and return that as value of State.
--   - If the value is false, findM will tr the next item.
--   - If true, then we can return the tuple.
--
-- Reminders:
-- =<< :: Bind m => (a -> m b) -> m a -> m b
--
findMTest1 :: (Optional Char, Int)
-- findMTest1 = let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- Equivalent:
findMTest1 = runState (findM
                       (\x -> (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get)
                       (listh ['a'..'h'])) 0

-- We can create functions via `let` with arguments. For example, the function
-- `f` below takes an Integer as the first argument, bound to var `a`.
testLet1 :: Integer
testLet1 = let f a = (a+1) in f 0

-- findMTest2 = let p = (\s -> (const $ pure ('c' == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0


-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat xs = eval
                  -- Simliar solutions:
                  -- (findM (\x -> get >>= (\s -> pure (S.member x s) >>= (\y -> State (\s' -> (y, S.insert x s'))))) xs)
                  -- (findM (\x -> get >>= (\s -> if (S.member x s) then pure True else put (S.insert x s) >>= (\_ -> pure False))) xs)
                  -- (findM (\x -> get >>= (\s -> let mem = (S.member x s) in put (S.insert x s) >>= (\_ -> pure mem))) xs)
                  -- (findM (\x -> get >>= (\s -> let mem = (S.member x s) in (const mem) <$> put (S.insert x s))) xs)
                  (findM (\x -> State (\s -> let b = (S.member x s) in (b, S.insert x s))) xs)
                  S.empty

-- Above, the predicate passed into findM works by creating a State continuation
-- that:
--
-- - First `get`s the default init state (S.empty)
-- - Then does a member check, whose value (true or false) is inserted into
--   the State (really a continuation, a new State), then the updated set with
--   that member is also inserted into the state.
--
-- Notes:
-- findM :: Monad f => (a -> f Bool) -> List a -> f (Optional a)
-- >>= :: Bind m => m a -> (a -> m b) -> m b
-- (<$>) :: (a -> b) -> State s a -> State s b
-- (<*>) :: State s (a -> b) -> State s a -> State s b


-- Notes:
-- filtering :: Applicative f => (a -> f Bool) -> List a -> f (List a)

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> firstRepeat (distinct xs) == Empty
--
-- prop> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct ::
  Ord a =>
  List a
  -> List a
distinct xs = eval
              (filtering
                (\x -> State (\s -> (S.notMember x s, S.insert x s)))
                xs)
              S.empty

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `findM` with `State` and `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
--
-- The idea behind this implementation is to produce an infinite sequence of sum
-- of squares of the digits. Then, we use findM to look for either a presence of
-- 1 (a happy number) or of a repeating member (a sad number). Once either is
-- found, we say we've found the item. Finally, we check to see if it contains 1
-- (happy) instead of another number (repeating).
--
isHappy ::
  Integer
  -> Bool
isHappy i = let result = runState (findM (\x -> State (\s -> (x == 1 || S.member x s, S.insert x s))) (produce sumOfSquare i)) S.empty
            in contains 1 (fst result)

sumOfSquare :: Integer -> Integer
sumOfSquare x = if x < 10 then square x else sumOfSquare' 0 x

sumOfSquare' :: Integer -> Integer -> Integer
sumOfSquare' s 0 = s
sumOfSquare' s x = sumOfSquare' (s + (square $ mod x 10)) (div x 10)

square :: Integer -> Integer
square = join (*)

asInt :: P.String -> P.String
asInt = id

-- findM :: (a -> f Bool) -> List a -> f (Optional a)
-- produce :: (a -> a) -> a -> List a
-- join :: Bind f => f (f a) -> f a
-- contains :: Eq a => a -> Optional a -> Bool
