Haskell Protocol Buffer Compiler - Source Code Annotations
==========================================================

    Copyright © 2014 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

Annotations expand the context of a protocol buffer entity with
an additional list of contexts for the entity's various constituents,
identified by index paths as described in descriptor.proto.

> module ProtocolBuffers.Annotations (
>   HasAnnotations (..),
>   Annotations, Annotated (..),
>   componentContexts, annotated, (@@/), (+@@), (@@+), (++@@), (@@++), ($@@)
> ) where

> import Control.Applicative
> import Control.Monad
> import Data.Functor
> import Data.Int
> import Data.Monoid
> import ProtocolBuffers.Contexts
> import ProtocolBuffers.Positions
> import ProtocolBuffers.Utilities

> infixl 8 :@@+, @@/, +@@, @@+, ++@@, @@++
> infixr 0 $@@

> class HasAnnotations a where
>   annotations :: a -> Annotations
>   amap :: (Annotations -> Annotations) -> a -> a

> newtype Annotations = Annotations { _componentContexts :: [Contextual IndexPath] } deriving (Eq, Ord)

> instance HasAnnotations Annotations where
>   annotations = id
>   amap f = f

> instance Show Annotations where
>   showsPrec p = showList . componentContexts

> data Annotated a = !(Contextual a) :@@+ !Annotations deriving (Eq, Ord, Show)

> instance HasAnnotations (Annotated a) where
>   annotations (cx :@@+ as) = annotations as
>   amap f (cx :@@+ as) = cx :@@+ f as

> instance HasContext (Annotated a) where
>   context (cx :@@+ as) = context cx
>   cmap f (cx :@@+ as) = cmap f cx :@@+ as

> instance HasPosition (Annotated a) where
>   position (cx :@@+ as) = position cx
>   pmap f (cx :@@+ as) = pmap f cx :@@+ as

> instance HasValue Annotated where
>   value (cx :@@+ as) = value cx

> instance Functor Annotated where
>   fmap f (cx :@@+ as) = fmap f cx :@@+ as
>   x <$ (cx :@@+ as) = (x <$ cx) :@@+ as

> componentContexts :: HasAnnotations a => a -> [Contextual IndexPath]
> componentContexts = _componentContexts . annotations

> annotated :: Contextual a -> Annotated a
> annotated x = x :@@+ Annotations []

> (@@/) :: HasAnnotations a => a -> IndexPath -> a
> x @@/ ip = amap (Annotations . map (fmap (ip ++)) . componentContexts) x

> (+@@) :: HasAnnotations a => Contextual IndexPath -> a -> a
> cip +@@ x = amap (Annotations . (cip :) . componentContexts) x

> (@@+) :: HasAnnotations a => a -> Contextual IndexPath -> a
> x @@+ cip = amap (Annotations . (++ [cip]) . componentContexts) x

> (@@++) :: HasAnnotations a => a -> Annotations -> a
> x @@++ as = amap (Annotations . (++ componentContexts as) . componentContexts) x

> (++@@) :: HasAnnotations a => Annotations -> a -> a
> as ++@@ x = amap (Annotations . (componentContexts as ++) . componentContexts) x

> ($@@) :: (Annotated a -> Annotated b) -> a -> Annotated b
> f $@@ x = f $ annotated $ x :@@ noContext
