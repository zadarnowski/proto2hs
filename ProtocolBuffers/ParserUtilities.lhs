> {-# LANGUAGE OverloadedStrings #-}

> module ProtocolBuffers.ParserUtilities (
>   produceFileDescriptor,
>   appendFileDescriptorProtoMessageType,
>   produceMessageDefinition,
>   appendDescriptorNestedType,
>   produceFieldStatement,
>   produceUnlabeledFieldStatement,
> ) where

> import Data.Int
> import ProtocolBuffers.Annotations
> import ProtocolBuffers.Contexts
> import ProtocolBuffers.Positions
> import ProtocolBuffers.Syntax
> import ProtocolBuffers.Tokens
> import ProtocolBuffers.Utilities

  Prepare an annotated entity for inclusion as a component of another entity.
  All existing annotations are prefixed with the specified index path and
  a new annotation is added for the complete entity itself.

> nestedComponent :: IndexPath -> Annotated a -> Annotated a
> nestedComponent ip x = [] :@@ context x +@@ x @@/ ip

> prependComponent :: IndexPath -> (a -> b -> b) -> Annotated a -> Annotated b -> Annotated b
> prependComponent ip uf c x = annotations (nestedComponent ip c) ++@@ fmap (uf (value c)) x

> appendComponent :: IndexPath -> (b -> a -> a) -> Annotated a -> Annotated b -> Annotated a
> appendComponent ip uf x c = fmap (uf (value c)) x @@++ annotations (nestedComponent ip c)

> appendRepeatedComponent :: IndexPath -> (a -> [b]) -> ([b] -> a -> a) -> Annotated a -> Annotated b -> Annotated a
> appendRepeatedComponent ip ef uf x c = appendComponent (ip ++ [fromIntegral $ length cs]) uf' x c
>  where
>   uf' c x = uf (cs ++ [c]) x
>   cs = ef (value x)

 checkSyntax :: Contextual ByteString -> P ()
 checkSyntax s
   | (value s == "proto2") = return ()
   | otherwise = complain (position s) $ "Unsupported syntax specified: " ++ value s ++ " (expecting \"proto2\".)"

> produceFileDescriptor :: (Annotated FileDescriptorProto -> Annotated FileDescriptorProto) -> FilePath -> FileDescriptorProto
> produceFileDescriptor fds fp = (value fd) { fileDescriptorProtoSourceCodeInfo = Just si }
>  where
>   fd = fds $ annotated $ defaultFileDescriptorProto :@@ between sp sp
>   sp = () :@ startPositionInFile fp
>   si = defaultSourceCodeInfo { sourceCodeInfoLocations = map produceLocation (componentContexts fd) }
>
>   produceLocation :: Contextual IndexPath -> SourceCodeInfoLocation
>   produceLocation as = defaultSourceCodeInfoLocation {
>     sourceCodeInfoLocationPath                = value as,
>     sourceCodeInfoLocationSpan                = produceSpan $ context as,
>     sourceCodeInfoLocationLeadingComments     = value $ leadingContext as,
>     sourceCodeInfoLocationTrailingComments    = value $ trailingContext as
>   }
>
>   produceSpan :: Context -> [Index]
>   produceSpan as
>     | (ll == tl) = [ ll - 1, columnNumber lp - 1, columnNumber tp - 1 ]
>     | otherwise  = [ ll - 1, columnNumber lp - 1, tl - 1, columnNumber tp - 1 ]
>    where
>     ll = lineNumber lp
>     lp = position lx
>     lx = leadingContext as
>     tl = lineNumber tp
>     tp = position tx
>     tx = trailingContext as

> appendFileDescriptorProtoMessageType :: Annotated FileDescriptorProto -> Annotated DescriptorProto -> Annotated FileDescriptorProto
> appendFileDescriptorProtoMessageType =
>   appendRepeatedComponent [4] fileDescriptorProtoMessageTypes $ \ ds fd -> fd { fileDescriptorProtoMessageTypes = ds }

> appendFileDescriptorProtoEnumType :: Annotated FileDescriptorProto -> Annotated DescriptorProto -> Annotated FileDescriptorProto
> appendFileDescriptorProtoEnumType =
>   appendRepeatedComponent [5] fileDescriptorProtoEnumTypes $ \ es fd -> fd { fileDescriptorProtoEnumTypes = es }

> appendFileDescriptorProtoService :: Annotated FileDescriptorProto -> Annotated EnumDescriptorProto -> Annotated FileDescriptorProto
> appendFileDescriptorProtoService =
>   appendRepeatedComponent [6] fileDescriptorProtoEnumTypes $ \ es fd -> fd { fileDescriptorProtoEnumTypes = es }

> appendFileDescriptorProtoExtension :: Annotated FileDescriptorProto -> Annotated DescriptorProto -> Annotated FileDescriptorProto
> appendFileDescriptorProtoExtension =
>   appendRepeatedComponent [7] fileDescriptorProtoEnumTypes $ \ es fd -> fd { fileDescriptorProtoEnumTypes = es }






> produceMessageDefinition :: Annotated Name -> Annotated DescriptorProto -> Annotated DescriptorProto
> produceMessageDefinition = prependComponent [1] $ \ n d -> d { descriptorProtoName = Just n }

> appendDescriptorNestedType :: Annotated DescriptorProto -> Annotated DescriptorProto -> Annotated DescriptorProto
> appendDescriptorNestedType =
>   appendRepeatedComponent [3] descriptorProtoNestedTypes $ \ ds d -> d { descriptorProtoNestedTypes = ds }

> appendDescriptorField :: Annotated DescriptorProto -> Annotated FieldDescriptorProto -> Annotated DescriptorProto
> appendDescriptorField =
>   appendRepeatedComponent [2] descriptorProtoFields $ \ fs d -> d { descriptorProtoFields = fs }

> produceFieldStatement :: Annotated FieldDescriptorProtoLabel -> Annotated FieldDescriptorProto -> Annotated FieldDescriptorProto
> produceFieldStatement = prependComponent [4] $ \ l fd -> fd { fieldDescriptorProtoLabel = Just l }

> produceUnlabeledFieldStatement ::
>   Annotated (Either FieldDescriptorProtoType QualifiedName) ->
>   Annotated Name ->
>   Annotated Int32 ->
>   Annotated FieldOptions ->
>   Either Context (Annotated DescriptorProto) ->
>   Annotated FieldDescriptorProto
> produceUnlabeledFieldStatement t n i os gd =
>   prependFieldDescriptorProtoType t $
>   prependComponent [1] (\ n fd -> fd { fieldDescriptorProtoName = Just n } ) n $
>   prependComponent [3] (\ i fd -> fd { fieldDescriptorProtoNumber = Just i }) i $
>   prependFieldOptions os $@@
>   defaultFieldDescriptorProto -- TODO: deal with group or trailing context

> prependFieldDescriptorProtoType ::
>   Annotated (Either FieldDescriptorProtoType QualifiedName) -> Annotated FieldDescriptorProtoType -> Annotated FieldDescriptorProtoType
> prependFieldDescriptorProtoType ft fd =
>   case value ft of
>     Left t  -> prependComponent [5] (\ t fd -> fd { fieldDescriptorProtoType = Just t }) (t <$ ft) fd
>     Right n -> prependComponent [6] (\ n fd -> fd { fieldDescriptorProtoTypeName = Just n }) (n <$ ft) fd

> parseFieldType :: QualifiedName -> Either FieldDescriptorProtoType QualifiedName
> parseFieldType ["double"]   = Left TYPE_DOUBLE
> parseFieldType ["float"]    = Left TYPE_FLOAT
> parseFieldType ["int64"]    = Left TYPE_INT64
> parseFieldType ["uint64"]   = Left TYPE_UINT64
> parseFieldType ["int32"]    = Left TYPE_INT32
> parseFieldType ["fixed64"]  = Left TYPE_FIXED64
> parseFieldType ["fixed32"]  = Left TYPE_FIXED32
> parseFieldType ["bool"]     = Left TYPE_BOOL
> parseFieldType ["string"]   = Left TYPE_STRING
> parseFieldType ["group"]    = Left TYPE_GROUP
> parseFieldType ["message"]  = Left TYPE_MESSAGE
> parseFieldType ["bytes"]    = Left TYPE_BYTES
> parseFieldType ["uint32"]   = Left TYPE_UINT32
> parseFieldType ["enum"]     = Left TYPE_ENUM
> parseFieldType ["sfixed32"] = Left TYPE_SFIXED32
> parseFieldType ["sfixed64"] = Left TYPE_SFIXED64
> parseFieldType ["sint32"]   = Left TYPE_SINT32
> parseFieldType ["sint64"]   = Left TYPE_SINT64
> parseFieldType qn           = Right qn

  Parse a field number, converting invalid (i.e., non-positive and huge) numbers into -1
  to allow detection of semantic errors during semantic analysis:

> parseFieldNumber :: Integer -> Int32
> parseFieldNumber x
>   | (0 < x && x < 2^31) = fromInteger x
>   | otherwise = -1


> produceFieldOptions = error "TODO"

