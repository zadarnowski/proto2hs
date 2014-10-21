> module ProtocolBuffers.Generator (
> ) where

> generateMessageType :: MessageDescriptor -> Builder
> generateMessageType md =
>   mconcat $ intersperse blank [typeDefinition, defaultValue, encoder, decoder]

>   "data " <> stringUtf8 (marshallTypeName n) <> " = " <> stringUtf (marshallConstructorName n) <> " {" <> nl <>
>   tab <> mconcat (intersperse (nl <> tab) $ map generateMessageField $ messageDescriptorFields md) <> nl
>   "} deriving (Eq, Ord, Show, Typeable)" <> nl <>
>   nl <>
>   stringUtf (marshallDefaultName n) <> " :: " <> stringUtf8 (marshallTypeName n) <> nl
>   stringUtf (marshallDefaultName n) <> " = " <> stringUtf (marshallConstructorName n) <> " {" <> nl <>
>   ht <> mconcat (intersperse (nl <> ht) $ map generateDefaultMessageField $ messageDescriptorFields md) <> nl
>   "} deriving (Eq, Ord, Show, Typeable)" <> nl <>

>  where

>   typeDefinition =
>   defaultValue =

>   encoder =
>     ename <> " :: " <> tname <> " -> Builder" <> nl <>
>     ename <> " x = runSTUArray $ do" <> nl <>
>     tab <> "a <- newArray (0, n) 0" <> nl <>
>     -- TODO... set default values
>     tab <> "

>   decoder =
>     dname <> " :: ByteString -> Either [" <> decoderErrorName <> "] " <> tname <> nl <>
>     dname <> " s = " <> nl <>
>     

>   decoderError =
>     "data " <> decoderResultName <> " = " <> nl <>
>     tab <> 

>   name = messageDescriptorName md




> generateMessageField :: MessageField -> Builder
> generateMessageField mf =
>   generateType


> tab, nl, nli, blank :: Builder
> tab   = "    "
> nl    = char7 '\n'
> blank = "\n\n"
