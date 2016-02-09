# Notes

## Extend and Comonad

Comonad is the "dual" of Bind; the function arrows are flipped from Bind:

```haskell
=<< :: (a -> f b) -> f a -> f b -- Bind
<<= :: (f a -> b) -> f a -> f b -- Extend

pure   :: a -> f a
copure :: f a -> a
```


[Comonad](https://wiki.haskell.org/Typeclassopedia#Comonad)

## State and StateT

In Haskell we often keep state by carrying values around. A `State` looks like this:

```
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }
```

It runs a computation that spits out a value and a new state. The value `a` is
derived from the context the `State` runs in. For example, to make a list
unique, we can use `filtering` and carry around a `State` with a `Set` of all
the seen-before elements:

```haskell
distinct :: Ord a => List a -> List a
distinct xs = eval (filtering
                (\x -> State (\s -> (S.notMember x s, S.insert x s))) xs) S.empty
```

We have `StateT` if we need the state and value to be wrapped in some other
outer computation. In the Logging example in StateT exercises, we need to grab
distinct elements AND log stuff (in a list). This requires two states to be
carried around, the set of seen-before elements and the log. Logging provides a
hole to put the log in, but we need to put that Logging in the StateT.

## Monad

`Monad` type-class is the combination of `Applicative` and `Bind`. Namely,
`pure` from Applicative is Prelude Monad's `return`. And from Bind we get `>>=`/`=<<`.

So that means Monads also get Functor's `<$>` (fmap) and Applicative's `<*>` (apply).

## Bind

Bind (`=<<`) has the type `(a -> f b) -> f a -> f b`. You can create `<*>` in
terms of `=<<` and `<$>`.

You can compose functions in the Bind context. This is the function `<=<`,
pronounced *kleisli composition*. The implementation is quite simple:

```
(<=<) :: Bind f => (b -> f c) -> (a -> f b) -> a -> f c
(<=<) f g a = f =<< (g a)
```

See Bind.hs for details.

## Applicative

You can implement `<$>` (fmap from Functor) in terms of `<*>` and `pure`.

Lifting a function can act almost like an accumulator in a computational
context.  For example, say you have: `(+ 10) <$> Full 3`. But say you want to
add a bunch of numbers together. Look at the this implementation below. Notice
the recurisve call, and 'as' as the accumulating list, which is then returned
in the f context:

```
sequence ::
  Applicative f =>
  List (f a)
  -> f (List a)
sequence Nil = pure Nil
sequence (f :. fs) = (\a as -> a :. as) <$> f <*> sequence fs
```

## Apply

Apply is partially towards Applicative, leaving out `pure`. So Apply only gets
<*> (apply) of type `f (a -> b) -> f a -> f b`.

A most interesting thing, however, is that you can implement `<*>` purely in
terms of `fmap` and `bind`. That is, if a data type implements Bind and
Functor, you get Apply. See my `Id` implementation's notes.

To **lift** an n-nary function is to allow that function to be applied in the
given context. For example, `(+)` is a binary function. If we want to apply
that to two Optionals: `(+) <$> Full 3 <*> Full 4` returns `Full 7`. We can
also lift functions that return functions: `((+) <$> sum <*> length)` returns a
unary function taking a list: `(+) <$> sum <*> length (1 :. 2 :. 3 :. Nil)`
returns `9`.

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
- [x] Course.Apply
- [x] Course.Applicative
- [x] Course.Bind
- [x] Course.Monad (please see this issue)
- [x] Course.FileIO
- [x] Course.State
- [x] Course.StateT
- [x] Course.Extend
- [x] Course.Comonad
- [ ] SKIP Course.Compose
- [x] Course.Traversable
- [ ] Course.ListZipper
  - [ ] Optimize against solution
- [x] Course.Extend
- [x] Course.Parser (see also Course.Person for the parsing rules)
- [x] Course.MoreParser
- [ ] Course.JsonParser
- [ ] Course.Interactive
- [ ] Course.Anagrams
- [ ] Course.FastAnagrams
- [ ] Course.Cheque

