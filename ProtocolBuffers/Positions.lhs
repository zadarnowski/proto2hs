Haskell Protocol Buffer Compiler - Source Code Positions
========================================================

    Copyright © 2014 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

A class used to keep track of file positions. In protocol buffer source info format
the file name is not stored, but we keep track of it anyway for convenience.
Also unlike protocol buffers, we count line and column numbers from one not zero,
and fix tab stops eight columns apart. However,we still use 32-bit signed integer
types for all line and column numbers in alignment with protocol buffers.

> module ProtocolBuffers.Positions (
>   Index,
>   HasPosition (..),
>   Position, Positioned (..),
>   noPosition,
>   lineNumber, columnNumber,
>   startPositionInFile, advanceLine, advanceColumn,
>   nextLine, nextColumn, nextTab,
>   tabWidth
> ) where

> import Data.Functor
> import Data.Int
> import ProtocolBuffers.Utilities

> infixl 8 :@

> class HasPosition a where
>   position :: a -> Position
>   pmap :: (Position -> Position) -> a -> a

  Note that we store line position separateley for efficiency,
  since it changes much less often than column positions!

> data Position = POS LinePosition !Index deriving (Eq, Ord)
> data LinePosition = LPOS FilePath !Index deriving (Eq, Ord)

> instance Show Position where
>   showsPrec _ (POS (LPOS fp ln) cn) = showString fp . showLine
>     where showLine = if ln > 0 then showChar ':' . shows ln . showColumn else id
>           showColumn = if cn > 0 then showChar ':' . shows cn else id

> instance HasPosition Position where
>   position = id
>   pmap f = f

> data Positioned a = !a :@ !Position deriving (Eq, Ord, Show)

> instance HasPosition (Positioned a) where
>   position (_ :@ p) = p
>   pmap f (x :@ p) = x :@ f p

> instance HasValue Positioned where
>   value (v :@ p) = v

> instance Functor Positioned where
>   fmap f (x :@ p) = f x :@ p
>   x <$ (_ :@ p) = x :@ p

> noPosition :: Position
> noPosition = POS (LPOS "" 0) 0

> lineNumber :: Position -> Index
> lineNumber (POS (LPOS _ ln) cn) = ln

> columnNumber :: Position -> Index
> columnNumber (POS (LPOS _ ln) cn) = cn

> startPositionInFile :: FilePath -> Position
> startPositionInFile fp = POS (LPOS fp 1) 1

> advanceLine :: Index -> Position -> Position
> advanceLine n (POS (LPOS fp ln) cn) = POS (LPOS fp (ln + n)) 1

> advanceColumn :: Index -> Position -> Position
> advanceColumn n (POS lp cn) = POS lp (cn + n)

> nextLine :: Position -> Position
> nextLine = advanceLine 1

> nextColumn :: Position -> Position
> nextColumn = advanceColumn 1

> nextTab :: Position -> Position
> nextTab (POS lp cn) = POS lp ((cn - 1) `div` tabWidth * tabWidth + tabWidth + 1)

> tabWidth :: Index
> tabWidth = 8
