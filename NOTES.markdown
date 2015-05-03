# Notes

## Functor

Note the comments on the functor implementation goes like "maps a function on
the f functor", which seems circular. When you say "the list functor", you're
saying "the list implementation of the Functor typeclass."

The functor laws seems to enforce the idea that you shouldn't do anything
"outside" of the supplied function. So don't reverse the list or something.

As for the intution of *computation context*, the best way I see this right now
is to see the type of fmap: (a -> b) -> (f a -> f b). Which is to say: given a
function, fmap will give you a function (computation) that runs in the context
of f.

## List

Cool to find out that `reverse` can be written with a `foldLeft`. No recursion,
"stack"-based.

For `notReverse`, which is another fun theorem, my intution was that `reverse`
was the only possible function that would keep the properties specified. A
proof seems not too difficult, my idea being to assume such other functino
exist, and prove that it breaks some law. The answer's proof is based of
induction, which makes sense.

Also, `doctest`, which is quickcheck by looking at the comments, is a
fascinating way to test. Feels almost like theorem proving (though obviously
not since it doens't cover every possible case). Hard part seems to be defining
useful properties.

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
- [x] Course.List
- [x] Course.Functor
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

