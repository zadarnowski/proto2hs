Haskell Protocol Buffer Compiler - Source Code Contexts
=======================================================

    Copyright © 2014 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

Protocol buffer element contexts, which describe the precise range of locations
spanned by a given element, as well as its leading and trailing comments. Note that
it wouldn't make much sense to have a single element span multiple source files,
although we do not prohibit it here (nor do we prohibit end positions placed ahead
of the start position, and other such nonsense.)

> module ProtocolBuffers.Contexts (
>   HasContext (..),
>   Context, Contextual (..), Comment, noContext,
>   noComment, leadingContext, trailingContext, between, (<@@), (@@>)
> ) where

> import Control.Applicative
> import Control.Monad
> import Data.ByteString (ByteString)
> import Data.Functor
> import Data.Int
> import ProtocolBuffers.Positions
> import ProtocolBuffers.Utilities

> infixl 8 :@@, <@@, @@>

> class HasPosition a => HasContext a where
>   context :: a -> Context
>   cmap :: (Context -> Context) -> a -> a

> data Context = Context {
>   _leadingContext  :: !(Positioned Comment),
>   _trailingContext :: !(Positioned Comment)
> } deriving (Eq, Ord, Show)

> instance HasContext Context where
>   context = id
>   cmap f = f

> instance HasPosition Context where
>   position = position . leadingContext
>   pmap f = cmap $ \c -> Context { _leadingContext = pmap f (_leadingContext c), _trailingContext = pmap f (_trailingContext c) }

> data Contextual a = !a :@@ !Context deriving (Eq, Ord, Show)

> instance HasContext (Contextual a) where
>   context (_ :@@ a) = a
>   cmap f (x :@@ a) = x :@@ f a

> instance HasPosition (Contextual a) where
>   position = position . context
>   pmap f = cmap (pmap f)

> instance HasValue Contextual where
>   value (v :@@ _) = v

> instance Functor Contextual where
>   fmap f (x :@@ a) = f x :@@ a
>   x <$ (_ :@@ a) = x :@@ a

> instance HasContext (Positioned a) where
>   context = context . position
>   cmap f = pmap (position . leadingContext . f . context)

> instance HasContext Position where
>   context p = Context { _leadingContext = Nothing :@ p, _trailingContext = Nothing :@ p }
>   cmap f = position . f . context

> type Comment = Maybe ByteString

> noComment :: Comment
> noComment = Nothing

> noContext :: Context
> noContext = Context { _leadingContext = Nothing :@ noPosition, _trailingContext = Nothing :@ noPosition }

> leadingContext, trailingContext :: (HasContext a) => a -> Positioned Comment
> leadingContext = _leadingContext . context
> trailingContext = _trailingContext . context

> between :: (HasContext a, HasContext b) => a -> b -> Context
> between x y = Context { _leadingContext = leadingContext x, _trailingContext = trailingContext y }

> (<@@) :: HasContext a => Context -> a -> a
> c <@@ x = cmap (\c' -> c' { _leadingContext  = leadingContext c }) x

> (@@>) :: HasContext a => a -> Context -> a
> x @@> c = cmap (\c' -> c' { _trailingContext = trailingContext c }) x

