# Notes

## Optional

A "binding" function seems to have a type of `(a -> m b) -> m a -> m b`. Which
is similar to Monad's `>>=`. The only difference is the flipped argument:

```
(>>=)  :: m a -> (a -> m b) -> m b
```

Same idea as Course.ID, except using a Maybe-like data type, Optional.

## ID

Seems like `Monad` is similar to `Functor` and `Applicative`? They are related
somehow. These type classes, after all, share similar signatures. Note Functor's
`pure` and Monad's `return`, for example.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a -> m b -> m b
  m >> n = m >>= \_ -> n
```

# Progress

- [x] Course.Id
- [x] Course.Optional
- [x] Course.Validation
- [ ] Course.List
- [ ] Course.Functor
- [ ] Course.Apply
- [ ] Course.Applicative
- [ ] Course.Bind
- [ ] Course.Monad (please see this issue)
- [ ] Course.FileIO
- [ ] Course.State
- [ ] Course.StateT
- [ ] Course.Extend
- [ ] Course.Comonad
- [ ] Course.Compose
- [ ] Course.Traversable
- [ ] Course.ListZipper
- [ ] Course.Parser (see also Course.Person for the parsing rules)
- [ ] Course.MoreParser
- [ ] Course.JsonParser
- [ ] Course.Interactive
- [ ] Course.Anagrams
- [ ] Course.FastAnagrams
- [ ] Course.Cheque

