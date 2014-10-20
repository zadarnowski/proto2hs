Haskell Protocol Buffer Compiler - Descriptor Model
===================================================

    Copyright © 2014 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

Data types which describe semantically-meaningful
structure of .proto files.

> module ProtocolBuffers.Model (
> ) where

> import ProtocolBuffers.Utilities

> data FileDescriptor = FILE_DESCRIPTOR {
>   fileDescriptorName      :: Name,
>   fileDescriptorPackage   :: Name,
>   fileDescriptorImports   :: List ImportDescriptor,
>   fileDescriptorTypes     :: Map Name TypeDescriptor,
>   fileDescriptorServices  :: Map Name ServiceDescriptor
> } deriving (Show)

> data ImportDescriptor = IMPORT_DESCRIPTOR {
> } deriving (Show)

> data TypeDescriptor =
>   PRIMITIVE_TYPE_DESCRIPTOR { }
>   | MESSAGE_TYPE_DESCRIPTOR { }
>   | ENUMERATED_TYPE_DESCRIPTOR {
>       enumTypeDescriptorValues :: Map Name Integer
>   }
>   deriving (Show)

> data ServiceDescriptor = SERVICE_DESCRIPTOR {
> } deriving (Show)

