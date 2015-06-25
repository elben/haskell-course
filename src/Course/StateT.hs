{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
--
-- Just like `State`, note that there is only one field in the StateT (since
-- newtype only allows one field), and that field is the function `s -> f (a, s)`.
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int Optional Int) 0
-- Full (3,0)
--
-- This implements the function `s -> (a, s)` (that is, StateT s f) as a
-- functor, assuming `f` is a functor. But why do we write `Functor (StateT s
-- f)` instead of `Functor (StateT s f a)`? Because this is similar to
-- implementing, for example, `Maybe a`, we write: Functor Maybe. That way, the
-- type `a` is not fixed to one type, since the whole point of this is to
-- convert (a -> b).
instance Functor f => Functor (StateT s f) where
  (<$>) ::
    (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<$>) t (StateT sf) = StateT (\s -> (\(a,s') -> (t a, s')) <$> sf s)
-- (<$>) :: (a -> b) -> f a -> f b



-- How does this turn into a list of int pair?
--
-- `(+1) <$> (pure 2)` must be a StateT Int List Int, so must be that:
--
--   <$> :: (Int -> Int) -> StateT Int List Int -> StateT Int List Int
--
-- Thus, `(pure 2)` must use the implementation provided below, which returns
-- this State:
--
--   pure 2 = StateT (\s -> return (2, s))
--
-- So `(+1) <$> (pure 2)` looks like:
--
--   StateT (\s -> return ((+1) 2, s))
--   StateT (\s -> return (3, s))
--
-- Which `return` is used in `return (3, s)`? Since StateT is a function of type
-- s -> f (a, s), and we set `f` to be List, this means that:
--
--   StateT (\s -> return (3, s)) :: StateT Int List Int
--   StateT (\s -> [(3, s)])
--
-- And finally we run the state and get [(3,0)]. So it is the `return` that
-- converts it to a list.
--
testFmap1 :: List (Int, Int)
testFmap1 = runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0

testFmap2 :: Optional (Int, Int)
testFmap2 = runStateT ((+1) <$> (pure 2) :: StateT Int Optional Int) 0

-- | Implement the `Apply` instance for @StateT s f@ given a @Bind f@.
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Bind f => Apply (StateT s f) where
  (<*>) ::
    StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<*>) (StateT st1) (StateT st2) = StateT (\s -> st1 s >>= (\(t, s') -> (\(a, s'') -> (t a, s'')) <$> (st2 s')))

-- | Implement the `Applicative` instance for @StateT s f@ given a @Applicative f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
instance Monad f => Applicative (StateT s f) where
  pure ::
    a
    -> StateT s f a
  pure a = StateT (\s -> return (a, s))

-- | Implement the `Bind` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
instance Monad f => Bind (StateT s f) where
  (=<<) ::
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b
  (=<<) f (StateT st) = StateT (\s -> st s >>= (\(a, s') -> runStateT (f a) s'))

-- Because we implemented Apply, Applicative and Bind for (StateT s f), we get
-- Monad.
instance Monad f => Monad (StateT s f) where

-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s Id a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- Id ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' st = StateT (\s -> pure $ st s)

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' st s = runId $ runStateT st s

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT st s = snd <$> (runStateT st s)

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' st s = snd (runState' st s)

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT st s = fst <$> runStateT st s

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' st s = fst (runState' st s)

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Monad f =>
  StateT s f s
getT = StateT (\s -> return (s, s))

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Monad f =>
  s
  -> StateT s f ()
putT s = StateT (\_ -> return ((), s))

-- | Remove all duplicate elements in a `List`.
--
-- Notes:
-- filtering :: Applicative f => (a -> f Bool) -> List a -> f (List a)
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' xs = eval' (filtering (\x -> StateT (\s -> Id (S.notMember x s, S.insert x s))) xs) S.empty

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
--
-- Notes:
-- - filtering :: Applicative f => (a -> f Bool) -> List a -> f (List a)
-- - filtering uses the function (a -> f Bool) to check whether or not to
-- include the item in the new list. In our case, 'f' is Optional. So if it's
-- "Full True", for example, the item is filtered. But if it's "Empty", the
-- Empty branch is taken by the applicative filtering function.
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF xs = evalT (filtering (\x -> StateT (\s -> if x > 100 then Empty else Full (S.notMember x s, S.insert x s))) xs) S.empty

-- | An `OptionalT` is a functor of an `Optional` value.
--
-- It has two fields in the data type, f and a.

-- Equivalent:
-- data OptionalT f a = OptionalT (f (Optional a))
-- runOptionalT :: (OptionalT f a) -> f (Optional a)
-- runOptionalT (OptionalT foa) = foa
--
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) f (OptionalT foa) = OptionalT $ (\oa -> f <$> oa) <$> foa

-- | Implement the `Apply` instance for `OptionalT f` given a Apply f.
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Apply f => Apply (OptionalT f) where
  (<*>) (OptionalT ott) (OptionalT ota) = OptionalT $ ((\ot -> (\oa -> ot <*> oa)) <$> ott) <*> ota

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
--
-- >>> runOptionalT (pure 3 :: OptionalT List Int)
-- [Full 3]
instance Applicative f => Applicative (OptionalT f) where
  pure a = OptionalT $ pure (Full a)

-- | Implement the `Bind` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
--
-- Notes:
-- <$> :: (a -> b) -> f a -> f b
-- <*> :: f (a -> b) -> f a -> f b
-- (=<<) :: (a -> f b) -> f a -> f b
-- pure :: a -> f a
--
instance Monad f => Bind (OptionalT f) where
  (=<<) :: (a -> OptionalT f b) -> OptionalT f a -> OptionalT f b
  (=<<) t (OptionalT ota) = OptionalT $
    ota >>= (\oa -> case oa of
                      Full a -> runOptionalT (t a)
                      Empty  -> return Empty)

instance Monad f => Monad (OptionalT f) where

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) ::
    (a -> b)
    -> Logger l a
    -> Logger l b
  (<$>) f (Logger l a) = Logger l (f a)

-- | Implement the `Apply` instance for `Logger`.
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Apply (Logger l) where
  (<*>) ::
    Logger l (a -> b)
    -> Logger l a
    -> Logger l b
  (<*>) (Logger l f) (Logger l' a) = Logger (l ++ l') (f a)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
instance Applicative (Logger l) where
  pure :: a -> Logger l a
  pure = Logger Nil

-- | Implement the `Bind` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Bind (Logger l) where
  (=<<) :: (a -> Logger l b) -> (Logger l a) -> (Logger l b)
  (=<<) f (Logger l a) = let Logger l' b = f a
                         in Logger (l ++ l') b

instance Monad (Logger l) where

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 l a = Logger (l :. Nil) a

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- - filtering :: Applicative f => (a -> f Bool) -> List a -> f (List a)
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
--
-- Notes:
--
-- We need three information to be passed along as we go through each element:
--
-- 1. Previously seen elements: Set a
-- 2. The log: Logger Chars
-- 3. The items we keep: List a
--
-- We know we need a State to keep the Set of things we've seen to be used with
-- `filtering`. We also need Logging to keep the log. We can keep the final list
-- in this Logging context. Then, note that the type signature asks for an
-- `Optional (List a)`. Why? Because we must be able to terminate the
-- computation if any integer is > 100.
--
-- And because we need it to be an `Optional (List a)`, we use an OptionalT so
-- we can get a Logger context around it. So it will be a `Logger Chars
-- (Optional List a)`.
--
-- How is the computation done?
--
-- The workhorse is `filtering`. The filtering function we pass returns:
-- `StateT (Set a) (OptionalT (Loggers Chars)) Bool`
--
-- This is complex, but `filtering` takes that type and fmaps over the StateT,
-- whose fmap implementation will fmap over the OptionalT, whose fmap
-- implementation will grab the Bool. If True, then the item is added to the
-- filtered items.
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG as =
  runOptionalT (evalT (filtering (\a -> StateT (\s ->
    OptionalT (
      if a > 100
      then Logger (("aborting > 100: " ++ listh (show a)) :. Nil) Empty
      else Logger (if even a
                   then (("even number: " ++ listh (show a)) :. Nil)
                   else Nil)
                  (Full (S.notMember a s, S.insert a s))))) as) S.empty)

-- Shorter version:
-- distinctG as =
--   runOptionalT (evalT (filtering (\a -> StateT (\s ->
--     OptionalT $
--       if a > 100
--       then log1 ("aborting > 100: " ++ listh (show a)) Empty
--       else (if even a
--             then log1 ("even number: " ++ listh (show a))
--             else pure) (Full (S.notMember a s, S.insert a s)))) as) S.empty)

