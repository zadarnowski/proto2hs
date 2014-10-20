Haskell Protocol Buffer Compiler - Parse Tree
=============================================

    Copyright © 2014 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

Raw parse tree of .proto files, devoid of any abstraction
and used to isolate subsequent validation and code generation
stages from primitive parsing concerns.
code generation stages of translation.

> module ProtocolBuffers.Syntax where

> import Data.ByteString (ByteString)
> import Data.Maybe
> import ProtocolBuffers.Contexts
> import ProtocolBuffers.Tokens
> import ProtocolBuffers.Utilities

> data TopLevelStatement =
>   TOP_LEVEL_NULL_STATEMENT
>   | TOP_LEVEL_MESSAGE_DEFINITION  MessageDefinition
>   | TOP_LEVEL_ENUM_DEFINITION     EnumDefinition
>   | TOP_LEVEL_EXTEND_DEFINITION   ExtendDefinition
>   | TOP_LEVEL_SERVICE_DEFINITION  ServiceDefinition
>   | TOP_LEVEL_IMPORT_STATEMENT    ImportStatement
>   | TOP_LEVEL_PACKAGE_STATEMENT   PackageStatement
>   | TOP_LEVEL_OPTION_STATEMENT    OptionStatement
>   deriving (Show)

> data MessageDefinition = MESSAGE_DEFINITION {
>   messageDefinitionName           :: Contextual Name,
>   messageDefinitionStatements     :: List (Contextual MessageStatement)
> } deriving (Show)

> data MessageStatement =
>   NULL_MESSAGE_STATEMENT
>   | NESTED_MESSAGE_DEFINITION     MessageDefinition
>   | NESTED_ENUM_DEFINITION        EnumDefinition
>   | NESTED_EXTEND_DEFINITION      ExtendDefinition
>   | MESSAGE_FIELD_STATEMENT       FieldStatement
>   | MESSAGE_ONEOF_STATEMENT       OneOfStatement
>   | MESSAGE_EXTENSIONS_STATEMENT  ExtensionsStatement
>   | MESSAGE_OPTION_STATEMENT      OptionStatement
>   deriving (Show)

> data FieldStatement = FIELD_STATEMENT {
>   fieldStatementLabel             :: Contextual Label,
>   fieldStatementProperties        :: UnlabeledFieldStatement
> } deriving (Show)

> data UnlabeledFieldStatement = UNLABELED_FIELD_STATEMENT {
>   fieldStatementType              :: Contextual QualifiedName,
>   fieldStatementName              :: Contextual Name,
>   fieldStatementNumber            :: Contextual Integer,
>   fieldStatementOptions           :: List (Contextual OptionAssignment),
>   fieldStatementGroupBody         :: Contextual (Maybe (List (Contextual MessageStatement)))
> } deriving (Show)

> data Label =
>   OPTIONAL | REPEATED | REQUIRED
>   deriving (Enum, Show)

> data OneOfStatement = ONE_OF_STATEMENT {
>   oneOfStatementName              :: Contextual Name,
>   oneOfStatementFields            :: List (Contextual UnlabeledFieldStatement)
> } deriving (Show)

> data ExtensionsStatement = EXTENSIONS_STATEMENT {
>   extensionsStatementRanges       :: List (Contextual ExtensionRange)
> } deriving (Show)

> data ExtensionRange = EXTENSION_RANGE {
>   extensionRangeStart             :: Contextual Integer,
>   extensionRangeEnd               :: Contextual (Maybe Integer)
> } deriving (Show)

> data EnumDefinition = ENUM_DEFINITION {
>   enumDefinitionName              :: Contextual Name,
>   enumDefinitionStatements        :: List (Contextual EnumStatement)
> } deriving (Show)

> data EnumStatement =
>   NULL_ENUM_STATEMENT
>   | ENUM_CONSTANT_STATEMENT EnumConstant
>   | ENUM_OPTION_STATEMENT OptionStatement
>   deriving (Show)

> data EnumConstant = ENUM_CONSTANT {
>   enumConstantName                :: Contextual Name,
>   enumConstantValue               :: Contextual Integer,
>   enumConstantOptions             :: List (Contextual OptionAssignment)
> } deriving (Show)

> data ExtendDefinition = EXTEND_DEFINITION {
>   extendDefinitionType            :: Contextual QualifiedName,
>   extendDefinitionFields          :: List (Contextual FieldStatement)
> } deriving (Show)

> data ServiceDefinition = SERVICE_DEFINITION {
>   serviceDefinitionName           :: Contextual Name,
>   serviceDefinitionStatements     :: List (Contextual ServiceStatement)
> } deriving (Show)

> data ServiceStatement =
>   NULL_SERVICE_STATEMENT
>   | SERVICE_METHOD_STATEMENT ServiceMethod
>   | SERVICE_OPTION_STATEMENT OptionStatement
>   deriving (Show)

> data ServiceMethod = SERVICE_METHOD {
>   serviceMethodName               :: Contextual Name,
>   serviceMethodArgumentType       :: Contextual QualifiedName,
>   serviceMethodResultType         :: Contextual QualifiedName,
>   serviceMethodBody               :: Contextual (Maybe (List (Contextual ServiceMethodStatement)))
> } deriving (Show)

> data ServiceMethodStatement =
>   NULL_SERVICE_METHOD_STATEMENT
>   | SERVICE_METHOD_OPTION_STATEMENT OptionStatement
>   deriving (Show)

> data ImportStatement = IMPORT_STATEMENT {
>   importStatementKind             :: Maybe (Contextual ImportKind),
>   importStatementPath             :: Contextual ByteString
> } deriving (Show)

> data ImportKind =
>   PUBLIC | WEAK
>   deriving (Enum, Show)

> data PackageStatement = PACKAGE_STATEMENT {
>   packageStatementName            :: Contextual (QList (Contextual Name))
> } deriving (Show)

> data OptionStatement = OPTION_STATEMENT {
>   optionStatementAssignments      :: List (Contextual OptionAssignment)
> } deriving (Show)

> data OptionAssignment = OPTION_ASSIGNMENT {
>   optionAssignmentName            :: Contextual (QList (Contextual OptionNamePart)),
>   optionAssignmentValue           :: Tokens
> } deriving (Show)

> data OptionNamePart =
>   SIMPLE_OPTION_NAME Name
>   | EXTENDED_OPTION_NAME QualifiedName
>   | ANONYMOUS_OPTION_NAME
>   deriving (Show)

> data QualifiedName =
>   RELATIVE (QList (Contextual Name))
>   | ABSOLUTE (QList (Contextual Name))
>   deriving (Show)
