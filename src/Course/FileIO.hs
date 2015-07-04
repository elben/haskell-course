{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = getArgs >>= (\(a :. _) -> run a)

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run fp = getFile fp >>= (\(_,content) ->
                          let files = (lines content) in
                          getFiles files >>= printFiles)

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles Nil = return Nil
-- This was tricky to get right for me. I wanted to put 'return' in the first
-- lambda, but the key is to think of it like continuation-style programming (in
-- essense it is, since monads allow result of first computation to be inserted into
-- second computation).
--
-- If it's like CPS (continuation-passing style), then we pass `p` into the very
-- inner computation, where we can get to the rest of the list.
getFiles (f :. fs) = getFile f >>= (\p -> (getFiles fs >>= (\ps -> return $ p :. ps)))

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile fp =
  readFile fp >>= (\chs -> return (fp, chs))

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles Nil = return ()
printFiles (f :. fs) = printFile (fst f) (snd f) *> printFiles fs

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile fp chars =
  putStrLn $ "============ " ++ fp ++ "\n" ++ chars
