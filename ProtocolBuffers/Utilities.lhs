Haskell Protocol Buffer Compiler - Utilities
============================================

    Copyright © 2014 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

A class used to keep track of file positions. In protocol buffer source info format
the file name is not stored, but we keep track of it anyway for convenience.
Also unlike protocol buffers, we count line and column numbers from one not zero,
and fix tab stops eight columns apart. However,we still use 32-bit signed integer
types for all line and column numbers in alignment with protocol buffers.
Common utilities that don't belong anywhere else.

> module ProtocolBuffers.Utilities (
>   HasValue (..),
>   List, Index, IndexPath, Name,
>   PList (..), fromPList,
>   QList, fromQList, fromQListOpt,
>   RList, fromRList,
>   showByteString
> ) where

> import Data.ByteString (ByteString)
> import Data.Char
> import Data.Int
> import Data.Maybe

> import qualified Data.ByteString as ByteString
> import qualified Data.ByteString.UTF8 as UTF8

  For convenience, we define the class of "wrapped values", which provide a "value" method with
  no particular semantic significance, apart of a convenient name, other than the identity

    forall f . (f . value == value . fmap f)

> class Functor f => HasValue f where
>   value :: f a -> a

  I like prefix names for lists:

> type List a = [a]

  To align ourselves with the protocol buffers' own representation of array indexes and file offsets,
  we define "Index" as a 32-bit signed integer type, and "IndexPath" as a list of such indexes:

> type Index = Int32
> type IndexPath = [Index]

  Names and comments are represented by byte strings:

> type Name = ByteString

  Right-recursive lists with a distinguished leftmost element type,
  specialised into non-empty lists ("QList") which start with "()"
  and possibly-empty right-associative lists ("RList") in which the
  first element has the same type as the rest of the list:

> data PList a b = LIST !a | PList a b ::: b deriving (Eq, Ord)
> type QList a   = PList a a
> type RList a   = PList () a

> instance (Show a, Show b) => Show (PList a b) where
>   showsPrec _ (LIST x) = shows x
>   showsPrec _ (xs ::: x) = shows xs . showString " ::: " . shows x

> fromPList :: PList a b -> (a, [b])
> fromPList xs = fromPList' xs []
>  where
>   fromPList' (LIST x) rs = (x, rs)
>   fromPList' (xs ::: x) rs = fromPList' xs (x:rs)

> fromQList :: QList a -> [a]
> fromQList = uncurry (:) . fromPList

> fromQListOpt :: Maybe (QList a) -> [a]
> fromQListOpt = fromMaybe [] . fmap fromQList

> fromRList :: RList a -> [a]
> fromRList = snd . fromPList

  Show a byte string, like "showString":

> showByteString :: ByteString -> ShowS
> showByteString = showString . UTF8.toString
