> module ProtocolBuffers.Syntax where

> import Data.ByteString (ByteString)
> import Data.Int
> import Data.Word

> data FileDescriptorSet = FileDescriptorSet {
>   fileDescriptorSetFile                   :: [FileDescriptorProto]
> } deriving (Eq, Ord, Show)

> defaultFileDescriptorSet = FileDescriptorSet {
>   fileDescriptorSetFile                   = []
> }

> data FileDescriptorProto = FileDescriptorProto {
>   fileDescriptorProtoName                 :: Maybe ByteString,
>   fileDescriptorProtoPackage              :: Maybe ByteString,
>   fileDescriptorProtoDependencies         :: [ByteString],
>   fileDescriptorProtoPublicDependencies   :: [Int32],
>   fileDescriptorProtoWeakDependencies     :: [Int32],
>   fileDescriptorProtoMessageTypes         :: [DescriptorProto],
>   fileDescriptorProtoEnumTypes            :: [EnumDescriptorProto],
>   fileDescriptorProtoServices             :: [ServiceDescriptorProto],
>   fileDescriptorProtoExtensions           :: [FieldDescriptorProto],
>   fileDescriptorProtoOptions              :: Maybe FileOptions,
>   fileDescriptorProtoSourceCodeInfo       :: Maybe SourceCodeInfo
> } deriving (Eq, Ord, Show)

> defaultFileDescriptorProto = FileDescriptorProto {
>   fileDescriptorProtoName                 = Nothing,
>   fileDescriptorProtoPackage              = Nothing,
>   fileDescriptorProtoDependencies         = [],
>   fileDescriptorProtoPublicDependencies   = [],
>   fileDescriptorProtoWeakDependencies     = [],
>   fileDescriptorProtoMessageTypes         = [],
>   fileDescriptorProtoEnumTypes            = [],
>   fileDescriptorProtoServices             = [],
>   fileDescriptorProtoExtensions           = [],
>   fileDescriptorProtoOptions              = Nothing,
>   fileDescriptorProtoSourceCodeInfo       = Nothing
> }

> data DescriptorProto = DescriptorProto {
>   descriptorProtoName                     :: Maybe ByteString,
>   descriptorProtoFields                   :: [FieldDescriptorProto],
>   descriptorProtoExtensions               :: [FieldDescriptorProto],
>   descriptorProtoNestedTypes              :: [DescriptorProto],
>   descriptorProtoEnumTypes                :: [EnumDescriptorProto],
>   descriptorProtoExtensionRanges          :: [DescriptorProtoExtensionRange],
>   descriptorProtoOneofDecls               :: [OneofDescriptorProto],
>   descriptorProtoOptions                  :: Maybe MessageOptions
> } deriving (Eq, Ord, Show)

> defaultDescriptorProto = DescriptorProto {
>   descriptorProtoName                     = Nothing,
>   descriptorProtoFields                   = [],
>   descriptorProtoExtensions               = [],
>   descriptorProtoNestedTypes              = [],
>   descriptorProtoEnumTypes                = [],
>   descriptorProtoExtensionRanges          = [],
>   descriptorProtoOneofDecls               = [],
>   descriptorProtoOptions                  = Nothing
> }

> data DescriptorProtoExtensionRange = DescriptorProtoExtensionRange {
>   descriptorProtoExtensionRangeStart      :: Maybe Int32,
>   descriptorProtoExtensionRangeEnd        :: Maybe Int32
> } deriving (Eq, Ord, Show)

> defaultDescriptorProtoExtensionRange = DescriptorProtoExtensionRange {
>   descriptorProtoExtensionRangeStart      = Nothing,
>   descriptorProtoExtensionRangeEnd        = Nothing
> }

> data FieldDescriptorProto = FieldDescriptorProto {
>   fieldDescriptorProtoName                :: Maybe ByteString,
>   fieldDescriptorProtoNumber              :: Maybe Int32,
>   fieldDescriptorProtoLabel               :: Maybe FieldDescriptorProtoLabel,
>   fieldDescriptorProtoType                :: Maybe FieldDescriptorProtoType,
>   fieldDescriptorProtoTypeName            :: Maybe ByteString,
>   fieldDescriptorProtoExtendee            :: Maybe ByteString,
>   fieldDescriptorProtoDefaultValue        :: Maybe ByteString,
>   fieldDescriptorProtoOneofIndex          :: Maybe Int32,
>   fieldDescriptorProtoOptions             :: Maybe FieldOptions
> } deriving (Eq, Ord, Show)

> defaultFieldDescriptorProto = FieldDescriptorProto {
>   fieldDescriptorProtoName                = Nothing,
>   fieldDescriptorProtoNumber              = Nothing,
>   fieldDescriptorProtoLabel               = Nothing,
>   fieldDescriptorProtoType                = Nothing,
>   fieldDescriptorProtoTypeName            = Nothing,
>   fieldDescriptorProtoExtendee            = Nothing,
>   fieldDescriptorProtoDefaultValue        = Nothing,
>   fieldDescriptorProtoOneofIndex          = Nothing,
>   fieldDescriptorProtoOptions             = Nothing
> }

> data FieldDescriptorProtoType =
>   TYPE_DOUBLE |
>   TYPE_FLOAT |
>   TYPE_INT64 |
>   TYPE_UINT64 |
>   TYPE_INT32 |
>   TYPE_FIXED64 |
>   TYPE_FIXED32 |
>   TYPE_BOOL |
>   TYPE_STRING |
>   TYPE_GROUP |
>   TYPE_MESSAGE |
>   TYPE_BYTES |
>   TYPE_UINT32 |
>   TYPE_ENUM |
>   TYPE_SFIXED32 |
>   TYPE_SFIXED64 |
>   TYPE_SINT32 |
>   TYPE_SINT64
>   deriving (Enum, Eq, Ord, Show)

> defaultFieldDescriptorProtoType = TYPE_DOUBLE

> data FieldDescriptorProtoLabel =
>   LABEL_OPTIONAL |
>   LABEL_REQUIRED |
>   LABEL_REPEATED
>   deriving (Enum, Eq, Ord, Show)

> defaultFieldDescriptorProtoLabel = LABEL_OPTIONAL

> data OneofDescriptorProto = OneofDescriptorProto {
>   oneofDescriptorProtoName                :: Maybe String
> } deriving (Eq, Ord, Show)

> defaultOneofDescriptorProto = OneofDescriptorProto {
>   oneofDescriptorProtoName                = Nothing
> }

> data EnumDescriptorProto = EnumDescriptorProto {
>   enumDescriptorProtoName                 :: Maybe String,
>   enumDescriptorProtoValues               :: [EnumValueDescriptorProto],
>   enumDescriptorProtoOptions              :: Maybe EnumOptions
> } deriving (Eq, Ord, Show)

> defaultEnumDescriptorProto = EnumDescriptorProto {
>   enumDescriptorProtoName                 = Nothing,
>   enumDescriptorProtoValues               = [],
>   enumDescriptorProtoOptions              = Nothing
> }

> data EnumValueDescriptorProto = EnumValueDescriptorProto {
>   enumValueDescriptorProtoName            :: Maybe String,
>   enumValueDescriptorProtoNumber          :: Maybe Int32,
>   enumValueDescriptorProtoOptions         :: Maybe EnumValueOptions
> } deriving (Eq, Ord, Show)

> defaultEnumValueDescriptorProto = EnumValueDescriptorProto {
>   enumValueDescriptorProtoName            = Nothing,
>   enumValueDescriptorProtoNumber          = Nothing,
>   enumValueDescriptorProtoOptions         = Nothing
> }

> data ServiceDescriptorProto = ServiceDescriptorProto {
>   serviceDescriptorProtoName              :: Maybe String,
>   serviceDescriptorProtoMethods           :: [MethodDescriptorProto],
>   serviceDescriptorProtoOptions           :: Maybe ServiceOptions
> } deriving (Eq, Ord, Show)

> defaultServiceDescriptorProto = ServiceDescriptorProto {
>   serviceDescriptorProtoName              = Nothing,
>   serviceDescriptorProtoMethods           = [],
>   serviceDescriptorProtoOptions           = Nothing
> }

> data MethodDescriptorProto = MethodDescriptorProto {
>   methodDescriptorProtoName               :: Maybe String,
>   methodDescriptorProtoInputType          :: Maybe String,
>   methodDescriptorProtoOutputType         :: Maybe String,
>   methodDescriptorProtoOptions            :: Maybe MethodOptions
> } deriving (Eq, Ord, Show)

> defaultMethodDescriptorProto = MethodDescriptorProto {
>   methodDescriptorProtoName               = Nothing,
>   methodDescriptorProtoInputType          = Nothing,
>   methodDescriptorProtoOutputType         = Nothing,
>   methodDescriptorProtoOptions            = Nothing
> }

> data FileOptions = FileOptions {
>   fileOptionsJavaPackage                  :: Maybe String,
>   fileOptionsJavaOuterClassname           :: Maybe String,
>   fileOptionsJavaMultipleFiles            :: Bool,
>   fileOptionsJavaGenerateEqualsAndHash    :: Bool,
>   fileOptionsJavaStringCheckUtf8          :: Bool,
>   fileOptionsOptimizeFor                  :: FileOptionsOptimizeMode,
>   fileOptionsGoPackage                    :: Maybe String,
>   fileOptionsCcGenericServices            :: Bool,
>   fileOptionsJavaGenericServices          :: Bool,
>   fileOptionsPyGenericServices            :: Bool,
>   fileOptionsDeprecated                   :: Bool,
>   fileOptionsUninterpretedOptions         :: [UninterpretedOption]
> } deriving (Eq, Ord, Show)

> defaultFileOptions = FileOptions {
>   fileOptionsJavaPackage                  = Nothing,
>   fileOptionsJavaOuterClassname           = Nothing,
>   fileOptionsJavaMultipleFiles            = False,
>   fileOptionsJavaGenerateEqualsAndHash    = False,
>   fileOptionsJavaStringCheckUtf8          = False,
>   fileOptionsOptimizeFor                  = SPEED,
>   fileOptionsGoPackage                    = Nothing,
>   fileOptionsCcGenericServices            = False,
>   fileOptionsJavaGenericServices          = False,
>   fileOptionsPyGenericServices            = False,
>   fileOptionsDeprecated                   = False,
>   fileOptionsUninterpretedOptions         = []
> }

> data FileOptionsOptimizeMode =
>   SPEED |
>   CODE_SIZE |
>   LITE_RUNTIME
>   deriving (Enum, Eq, Ord, Show)

> defaultFileOptionsOptimizeMode = SPEED

> data MessageOptions = MessageOptions {
>   messageOptionsMessageSetWireFormat      :: Bool,
>   messageOptionsNoStandardDescriptorAccess :: Bool,
>   messageOptionsDeprecated                :: Bool,
>   messageOptionsUninterpretedOptions      :: [UninterpretedOption]
> } deriving (Eq, Ord, Show)

> defaultMessageOptions = MessageOptions {
>   messageOptionsMessageSetWireFormat      = False,
>   messageOptionsNoStandardDescriptorAccess = False,
>   messageOptionsDeprecated                = False,
>   messageOptionsUninterpretedOptions      = []
> }

> data FieldOptions = FieldOptions {
>   fieldOptionsCtype                       :: FieldOptionsCType,
>   fieldOptionsPacked                      :: Bool,
>   fieldOptionsLazy                        :: Bool,
>   fieldOptionsDeprecated                  :: Bool,
>   fieldOptionsExperimentalMapKey          :: Maybe String,
>   fieldOptionsWeak                        :: Bool,
>   fieldOptionsUninterpretedOptions        :: [UninterpretedOption]
> } deriving (Eq, Ord, Show)

> defaultFieldOptions = FieldOptions {
>   fieldOptionsCtype                       = STRING,
>   fieldOptionsPacked                      = False,
>   fieldOptionsLazy                        = False,
>   fieldOptionsDeprecated                  = False,
>   fieldOptionsExperimentalMapKey          = Nothing,
>   fieldOptionsWeak                        = False,
>   fieldOptionsUninterpretedOptions        = []
> }

> data FieldOptionsCType =
>   STRING |
>   CORD |
>   STRING_PIECE
>   deriving (Enum, Eq, Ord, Show)

> defaultFieldOptionsCType = STRING

> data EnumOptions = EnumOptions {
>   enumOptionsAllowAlias                   :: Bool,
>   enumOptionsDeprecated                   :: Bool,
>   enumOptionsUninterpretedOptions         :: [UninterpretedOption]
> } deriving (Eq, Ord, Show)

> defaultEnumOptions = EnumOptions {
>   enumOptionsAllowAlias                   = False,
>   enumOptionsDeprecated                   = False,
>   enumOptionsUninterpretedOptions         = []
> }

> data EnumValueOptions = EnumValueOptions {
>   enumValueOptionsDeprecated              :: Bool,
>   enumValueOptionsUninterpretedOptions    :: [UninterpretedOption]
> } deriving (Eq, Ord, Show)

> defaultEnumValueOptions = EnumValueOptions {
>   enumValueOptionsDeprecated              = False,
>   enumValueOptionsUninterpretedOptions    = []
> }

> data ServiceOptions = ServiceOptions {
>   serviceOptionsDeprecated                :: Bool,
>   serviceOptionsUninterpretedOptions      :: [UninterpretedOption]
> } deriving (Eq, Ord, Show)

> defaultServiceOptions = ServiceOptions {
>   serviceOptionsDeprecated                = False,
>   serviceOptionsUninterpretedOptions      = []
> }

> data MethodOptions = MethodOptions {
>   methodOptionsDeprecated                 :: Bool,
>   methodOptionsUninterpretedOptions       :: [UninterpretedOption]
> } deriving (Eq, Ord, Show)

> defaultMethodOptions = MethodOptions {
>   methodOptionsDeprecated                 = False,
>   methodOptionsUninterpretedOptions       = []
> }

> data UninterpretedOption = UninterpretedOption {
>   uninterpretedOptionNames                :: [UninterpretedOptionNamePart],
>   uninterpretedOptionIdentifierValue      :: Maybe String,
>   uninterpretedOptionPositiveIntValue     :: Maybe Word64,
>   uninterpretedOptionNegativeIntValue     :: Maybe Int64,
>   uninterpretedOptionDoubleValue          :: Maybe Double,
>   uninterpretedOptionStringValue          :: Maybe ByteString,
>   uninterpretedOptionAggregateValue       :: Maybe String
> } deriving (Eq, Ord, Show)


> defaultUninterpretedOption = UninterpretedOption {
>   uninterpretedOptionNames                = [],
>   uninterpretedOptionIdentifierValue      = Nothing,
>   uninterpretedOptionPositiveIntValue     = Nothing,
>   uninterpretedOptionNegativeIntValue     = Nothing,
>   uninterpretedOptionDoubleValue          = Nothing,
>   uninterpretedOptionStringValue          = Nothing,
>   uninterpretedOptionAggregateValue       = Nothing
> }

> data UninterpretedOptionNamePart = UninterpretedOptionNamePart {
>   uninterpretedOptionNamePartName         :: String,
>   uninterpretedOptionNamePartIsExtension  :: Bool
> } deriving (Eq, Ord, Show)

> defaultUninterpretedOptionNamePart = UninterpretedOptionNamePart {
>   uninterpretedOptionNamePartName         = undefined,
>   uninterpretedOptionNamePartIsExtension  = False
> }

> data SourceCodeInfo = SourceCodeInfo {
>   sourceCodeInfoLocations                 :: [SourceCodeInfoLocation]
> } deriving (Eq, Ord, Show)

> defaultSourceCodeInfo = SourceCodeInfo {
>   sourceCodeInfoLocations                 = []
> }

> data SourceCodeInfoLocation = SourceCodeInfoLocation {
>   sourceCodeInfoLocationPath              :: [Int32],
>   sourceCodeInfoLocationSpan              :: [Int32],
>   sourceCodeInfoLocationLeadingComments   :: Maybe ByteString,
>   sourceCodeInfoLocationTrailingComments  :: Maybe ByteString
> } deriving (Eq, Ord, Show)

> defaultSourceCodeInfoLocation = SourceCodeInfoLocation {
>   sourceCodeInfoLocationPath              = [],
>   sourceCodeInfoLocationSpan              = [],
>   sourceCodeInfoLocationLeadingComments   = Nothing,
>   sourceCodeInfoLocationTrailingComments  = Nothing
> }
