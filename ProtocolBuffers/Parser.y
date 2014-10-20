{

{-

Haskell Protocol Buffer Compiler - Parser
=========================================

    Copyright © 2014 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

Happy parser for .proto files, with minor generalisations.
The output is a raw parse tree data structure defined in the
"ParseTree" module.

-}

module ProtocolBuffers.Parser (Error, Grammar, parseWith, fileDescriptorGrammar) where

import Data.ByteString (ByteString)
import Data.Either
import Data.Functor
import Data.String

import ProtocolBuffers.Contexts
import ProtocolBuffers.Positions
import ProtocolBuffers.Syntax
import ProtocolBuffers.Tokens
import ProtocolBuffers.Utilities

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as UTF8

}

%name fileDescriptorGrammar file_descriptor
%tokentype { Either Position (Contextual Token) }
%monad { Grammar }
%lexer { lexer } { Left _ }
%error { syntaxError }

%token

  identifier_token      { Right ($$ @ (IDENTIFIER_TOKEN _ :@@ _)) }
  integer_token         { Right ($$ @ (INTEGER_TOKEN    _ :@@ _)) }
  rational_token        { Right ($$ @ (RATIONAL_TOKEN _ _ :@@ _)) }
  string_token          { Right ($$ @ (STRING_TOKEN     _ :@@ _)) }

  "enum"                { Right ($$ @ (ENUM_TOKEN         :@@ _)) }
  "extend"              { Right ($$ @ (EXTEND_TOKEN       :@@ _)) }
  "extensions"          { Right ($$ @ (EXTENSIONS_TOKEN   :@@ _)) }
  "import"              { Right ($$ @ (IMPORT_TOKEN       :@@ _)) }
  "max"                 { Right ($$ @ (MAX_TOKEN          :@@ _)) }
  "message"             { Right ($$ @ (MESSAGE_TOKEN      :@@ _)) }
  "oneof"               { Right ($$ @ (ONEOF_TOKEN        :@@ _)) }
  "option"              { Right ($$ @ (OPTION_TOKEN       :@@ _)) }
  "optional"            { Right ($$ @ (OPTIONAL_TOKEN     :@@ _)) }
  "package"             { Right ($$ @ (PACKAGE_TOKEN      :@@ _)) }
  "public"              { Right ($$ @ (PUBLIC_TOKEN       :@@ _)) }
  "repeated"            { Right ($$ @ (REPEATED_TOKEN     :@@ _)) }
  "required"            { Right ($$ @ (REQUIRED_TOKEN     :@@ _)) }
  "returns"             { Right ($$ @ (RETURNS_TOKEN      :@@ _)) }
  "rpc"                 { Right ($$ @ (RPC_TOKEN          :@@ _)) }
  "service"             { Right ($$ @ (SERVICE_TOKEN      :@@ _)) }
  "syntax"              { Right ($$ @ (SYNTAX_TOKEN       :@@ _)) }
  "to"                  { Right ($$ @ (TO_TOKEN           :@@ _)) }
  "weak"                { Right ($$ @ (WEAK_TOKEN         :@@ _)) }

  "{"                   { Right ($$ @ (LBRACE_TOKEN       :@@ _)) }
  "}"                   { Right ($$ @ (RBRACE_TOKEN       :@@ _)) }
  "["                   { Right ($$ @ (LBRACKET_TOKEN     :@@ _)) }
  "]"                   { Right ($$ @ (RBRACKET_TOKEN     :@@ _)) }
  "("                   { Right ($$ @ (LPAREN_TOKEN       :@@ _)) }
  ")"                   { Right ($$ @ (RPAREN_TOKEN       :@@ _)) }
  ";"                   { Right ($$ @ (SEMICOLON_TOKEN    :@@ _)) }
  ","                   { Right ($$ @ (COMMA_TOKEN        :@@ _)) }
  "."                   { Right ($$ @ (PERIOD_TOKEN       :@@ _)) }
  "="                   { Right ($$ @ (EQUALS_TOKEN       :@@ _)) }
  "-"                   { Right ($$ @ (MINUS_TOKEN        :@@ _)) }

%%

file_descriptor :: { List (Contextual TopLevelStatement) }:
    syntax_descriptor_opt top_level_statement_list_opt  { $2 }

syntax_descriptor_opt :: { () }:
    "syntax" "=" string ";"                             {% checkSyntax $3 }
  | {- empty -}                                         {% return () }

top_level_statement_list_opt :: { List (Contextual TopLevelStatement) }:
    top_level_statement_list                            { fromQList $1 }
  | {- empty -}                                         { [] }

top_level_statement_list :: { QList (Contextual TopLevelStatement) }:
    top_level_statement                                 { LIST $1 }
  | top_level_statement_list top_level_statement        { $1 ::: $2 }

top_level_statement :: { Contextual TopLevelStatement }:
    null_statement                                      { TOP_LEVEL_NULL_STATEMENT      <\$  $1 }
  | message_definition                                  { TOP_LEVEL_MESSAGE_DEFINITION  <\$> $1 }
  | enum_definition                                     { TOP_LEVEL_ENUM_DEFINITION     <\$> $1 }
  | extend_definition                                   { TOP_LEVEL_EXTEND_DEFINITION   <\$> $1 }
  | service_definition                                  { TOP_LEVEL_SERVICE_DEFINITION  <\$> $1 }
  | import_statement                                    { TOP_LEVEL_IMPORT_STATEMENT    <\$> $1 }
  | package_statement                                   { TOP_LEVEL_PACKAGE_STATEMENT   <\$> $1 }
  | option_statement                                    { TOP_LEVEL_OPTION_STATEMENT    <\$> $1 }

null_statement :: { Contextual () }:
    ";"                                                 { () <$ $1 }

message_definition :: { Contextual MessageDefinition }:
    "message" name message_block                        { context $1 <@@ (MESSAGE_DEFINITION $2 <\$> $3) }

message_block :: { Contextual (List (Contextual MessageStatement)) }:
    "{" message_statement_list_opt "}"                  { $2 :@@ between $1 $3 }

message_statement_list_opt :: { List (Contextual MessageStatement) }:
    message_statement_list                              { fromQList $1 }
  | {- empty -}                                         { [] }

message_statement_list :: { QList (Contextual MessageStatement) }:
    message_statement                                   { LIST $1 }
  | message_statement_list message_statement            { $1 ::: $2 }

message_statement :: { Contextual MessageStatement }:
    null_statement                                      { NULL_MESSAGE_STATEMENT        <\$ $1 }
  | message_definition                                  { NESTED_MESSAGE_DEFINITION     <\$> $1 }
  | enum_definition                                     { NESTED_ENUM_DEFINITION        <\$> $1 }
  | extend_definition                                   { NESTED_EXTEND_DEFINITION      <\$> $1 }
  | field_statement                                     { MESSAGE_FIELD_STATEMENT       <\$> $1 }
  | oneof_statement                                     { MESSAGE_ONEOF_STATEMENT       <\$> $1 }
  | extensions_statement                                { MESSAGE_EXTENSIONS_STATEMENT  <\$> $1 }
  | option_statement                                    { MESSAGE_OPTION_STATEMENT      <\$> $1 }

field_statement :: { Contextual FieldStatement }:
    label unlabeled_field_statement                     { context $1 <@@ (FIELD_STATEMENT $1 <\$> $2) }

label :: { Contextual Label }:
    "optional"                                          { OPTIONAL <\$ $1 }
  | "repeated"                                          { REPEATED <\$ $1 }
  | "required"                                          { REQUIRED <\$ $1 }

unlabeled_field_statement :: { Contextual UnlabeledFieldStatement }:
    qualified_name name "=" integer option_assignments group_body { UNLABELED_FIELD_STATEMENT $1 $2 $4 $5 $6 :@@ between $1 $6 }

group_body :: { Contextual (Maybe (List (Contextual MessageStatement))) }:
    message_block                                       { Just    <\$> $1 }
  | ";"                                                 { Nothing <\$  $1 }

oneof_statement :: { Contextual OneOfStatement }:
    "oneof" name "{" unlabeled_field_statement_list_opt "}" { ONE_OF_STATEMENT $2 $4 :@@ between $1 $5 }

unlabeled_field_statement_list_opt :: { List (Contextual UnlabeledFieldStatement) }:
    unlabeled_field_statement_list                      { fromQList $1 }
  | {- empty -}                                         { [] }

unlabeled_field_statement_list :: { QList (Contextual UnlabeledFieldStatement) }:
    unlabeled_field_statement                           { LIST $1 }
  | unlabeled_field_statement_list unlabeled_field_statement { $1 ::: $2 }

extensions_statement :: { Contextual ExtensionsStatement }:
    "extensions" extension_list_opt ";"                 { EXTENSIONS_STATEMENT $2 :@@ between $1 $3 }

extension_list_opt :: { List (Contextual ExtensionRange) }:
    extension_list                                      { fromQList $1 }
  | {- empty -}                                         { [] }

extension_list :: { QList (Contextual ExtensionRange) }:
    extension_range                                     { LIST $1 }
  | extension_list "," extension_range                  { $1 ::: $3 }

extension_range :: { Contextual ExtensionRange }:
    integer                                             { EXTENSION_RANGE $1 (Just    <\$> $1) :@@ between $1 $1 }
  | integer "to" integer                                { EXTENSION_RANGE $1 (Just    <\$> $3) :@@ between $1 $3 }
  | integer "to" "max"                                  { EXTENSION_RANGE $1 (Nothing <\$  $3) :@@ between $1 $3 }

enum_definition :: { Contextual EnumDefinition }:
    "enum" name enum_block                              { context $1 <@@ (ENUM_DEFINITION $2 <\$> $3) }

enum_block :: { Contextual (List (Contextual EnumStatement)) }:
    "{" enum_statement_list_opt "}"                     { $2 :@@ between $1 $3 }

enum_statement_list_opt :: { List (Contextual EnumStatement) }:
    enum_statement_list                                 { fromQList $1 }
  | {- empty -}                                         { [] }

enum_statement_list :: { QList (Contextual EnumStatement) }:
    enum_statement                                      { LIST $1 }
  | enum_statement_list enum_statement                  { $1 ::: $2 }

enum_statement :: { Contextual EnumStatement }:
    null_statement                                      { NULL_ENUM_STATEMENT     <\$  $1 }
  | enum_constant                                       { ENUM_CONSTANT_STATEMENT <\$> $1 }
  | option_statement                                    { ENUM_OPTION_STATEMENT   <\$> $1 }

enum_constant :: { Contextual EnumConstant }:
    name "=" signed_integer option_assignments ";" { ENUM_CONSTANT $1 $3 $4 :@@ between $1 $5 }

extend_definition :: { Contextual ExtendDefinition }:
    "extend" qualified_name "{" field_statement_list_opt "}" { EXTEND_DEFINITION $2 $4 :@@ between $1 $5 }

field_statement_list_opt :: { List (Contextual FieldStatement) }:
    field_statement_list                                { fromQList $1 }
  | {- empty -}                                         { [] }

field_statement_list :: { QList (Contextual FieldStatement) }:
    field_statement                                     { LIST $1 }
  | field_statement_list field_statement                { $1 ::: $2 }

service_definition :: { Contextual ServiceDefinition }:
    "service" name service_block                        { context $1 <@@ (SERVICE_DEFINITION $2 <\$> $3) }

service_block :: { Contextual (List (Contextual ServiceStatement)) }:
    "{" service_statement_list_opt "}"                  { $2 :@@ between $1 $3 }

service_statement_list_opt :: { List (Contextual ServiceStatement) }:
    service_statement_list                              { fromQList $1 }
  | {- empty -}                                         { [] }

service_statement_list :: { QList (Contextual ServiceStatement) }:
    service_statement                                   { LIST $1 }
  | service_statement_list service_statement            { $1 ::: $2 }

service_statement :: { Contextual ServiceStatement }:
    null_statement                                      { NULL_SERVICE_STATEMENT   <\$  $1 }
  | service_method                                      { SERVICE_METHOD_STATEMENT <\$> $1 }
  | option_statement                                    { SERVICE_OPTION_STATEMENT <\$> $1 }

service_method :: { Contextual ServiceMethod }:
    "rpc" name "(" qualified_name ")" "returns" "(" qualified_name ")" service_method_body { SERVICE_METHOD $2 $4 $8 $10 :@@ between $1 $10 }

service_method_body :: { Contextual (Maybe (List (Contextual ServiceMethodStatement))) }:
    service_method_block                                { Just    <\$> $1 }
  | ";"                                                 { Nothing <\$  $1 }

service_method_block :: { Contextual (List (Contextual ServiceMethodStatement)) }:
    "{" service_method_statement_list_opt "}"           { $2 :@@ between $1 $3 }

service_method_statement_list_opt :: { List (Contextual ServiceMethodStatement) }:
    service_method_statement_list                       { fromQList $1 }
  | {- empty -}                                         { [] }

service_method_statement_list :: { QList (Contextual ServiceMethodStatement) }:
    service_method_statement                            { LIST $1 }
  | service_method_statement_list service_method_statement { $1 ::: $2 }

service_method_statement :: { Contextual ServiceMethodStatement }:
    null_statement                                      { NULL_SERVICE_METHOD_STATEMENT   <\$  $1 }
  | option_statement                                    { SERVICE_METHOD_OPTION_STATEMENT <\$> $1 }

import_statement :: { Contextual ImportStatement }:
    "import" import_kind string ";"                     { IMPORT_STATEMENT $2 $3 :@@ between $1 $4 }

import_kind :: { Maybe (Contextual ImportKind) }:
    "public"                                            { Just $ PUBLIC <\$ $1 }
  | "weak"                                              { Just $ WEAK   <\$ $1 }
  | {- empty -}                                         { Nothing }

package_statement :: { Contextual PackageStatement }:
    "package" relative_name ";"                         { PACKAGE_STATEMENT $2 :@@ between $1 $3 }

option_statement :: { Contextual OptionStatement }:
    "option" option_assignments ";"                     { OPTION_STATEMENT $2 :@@ between $1 $3 }

option_assignments :: { List (Contextual OptionAssignment) }:
    "[" option_assignment_list_opt "]"                  { $2 }
  | {- empty -}                                         { [] }

option_assignment_list_opt :: { List (Contextual OptionAssignment) }:
    option_assignment_list                              { fromQList $1 }
  | {- empty -}                                         { [] }

option_assignment_list :: { QList (Contextual OptionAssignment) }:
    option_assignment                                   { LIST $1 }
  | option_assignment_list "," option_assignment        { $1 ::: $3 }

option_assignment :: { Contextual OptionAssignment }:
    option_name_part_list "=" option_value              { OPTION_ASSIGNMENT $1 (value $3) :@@ between $1 $3 }

option_name_part_list :: { Contextual (QList (Contextual OptionNamePart)) }:
    option_name_part                                    { LIST $1 :@@ between $1 $1 }
  | option_name_part_list "." option_name_part          { (value $1 ::: $3) :@@ between $1 $3 }

option_name_part :: { Contextual OptionNamePart }:
    name                                                { SIMPLE_OPTION_NAME <\$> $1 }
  | "(" qualified_name ")"                              { EXTENDED_OPTION_NAME <\$> (context $1 <@@ $2 @@> context $3) }
  | "(" ")"                                             { ANONYMOUS_OPTION_NAME :@@ between $1 $2 }

option_value :: { Contextual Tokens }:
    alphanumeric_token                                  { ($1 ::> (END_OF_TOKENS $ position $ trailingContext $ context $ $1)) :@@ between $1 $1 }
  | "-" alphanumeric_token                              { ($1 ::> $2 ::> (END_OF_TOKENS $ position $ trailingContext $ context $ $2)) :@@ between $1 $2 }
  | string_token_list                                   { ((value $1) (END_OF_TOKENS $ position $ trailingContext $ context $ $1)) :@@ between $1 $1 }
  | uninterpreted_block                                 { ((value $1) (END_OF_TOKENS $ position $ trailingContext $ context $ $1)) :@@ between $1 $1 }

alphanumeric_token :: { Contextual Token }:
    identifier_token                                    { $1 }
  | keyword_token                                       { $1 }
  | integer_token                                       { $1 }
  | rational_token                                      { $1 }

string_token_list :: { Contextual (Tokens -> Tokens) }:
    string_token                                        { ($1 ::>) :@@ between $1 $1 }
  | string_token_list string_token                      { value $1 . ($2 ::>) :@@ between $1 $2 }

uninterpreted_block :: { Contextual (Tokens -> Tokens) }:
    "{" uninterpreted_element_list "}"                  { ($1 ::>) . value $2 . ($3 ::>) :@@ between $1 $3 }
  | "{" "}"                                             { ($1 ::>) . ($2 ::>) :@@ between $1 $2 }

uninterpreted_element_list :: { Contextual (Tokens -> Tokens) }:
    uninterpreted_element                               { $1 }
  | uninterpreted_element_list uninterpreted_element    { (value $1 . value $2) :@@ between $1 $2 }

uninterpreted_element :: { Contextual (Tokens -> Tokens) }:
    uninterpreted_token                                 { ($1 ::>) :@@ between $1 $1 }
  | uninterpreted_block                                 { $1 }

uninterpreted_token :: { Contextual Token }:
    identifier_token                                    { $1 }
  | integer_token                                       { $1 }
  | rational_token                                      { $1 }
  | string_token                                        { $1 }
  | keyword_token                                       { $1 }
  | "["                                                 { $1 }
  | "]"                                                 { $1 }
  | "("                                                 { $1 }
  | ")"                                                 { $1 }
  | ";"                                                 { $1 }
  | ","                                                 { $1 }
  | "."                                                 { $1 }
  | "="                                                 { $1 }
  | "-"                                                 { $1 }

qualified_name :: { Contextual QualifiedName }:
    relative_name                                       { RELATIVE <\$> $1 }
  | "." relative_name                                   { context $1 <@@ (ABSOLUTE <\$> $2) }

relative_name :: { Contextual (QList (Contextual Name)) }:
    name                                                { LIST $1 :@@ between $1 $1 }
  | relative_name "." name                              { (value $1 ::: $3) :@@ between $1 $3 }

name :: { Contextual Name }:
    identifier_token                                    { nameTokenValue <\$> $1 }
  | keyword_token                                       { nameTokenValue <\$> $1 }

keyword_token:
    "enum"                                              { $1 }
  | "extend"                                            { $1 }
  | "extensions"                                        { $1 }
  | "import"                                            { $1 }
  | "max"                                               { $1 }
  | "message"                                           { $1 }
  | "oneof"                                             { $1 }
  | "option"                                            { $1 }
  | "optional"                                          { $1 }
  | "package"                                           { $1 }
  | "public"                                            { $1 }
  | "repeated"                                          { $1 }
  | "required"                                          { $1 }
  | "returns"                                           { $1 }
  | "rpc"                                               { $1 }
  | "service"                                           { $1 }
  | "syntax"                                            { $1 }
  | "to"                                                { $1 }
  | "weak"                                              { $1 }

signed_integer :: { Contextual Integer }:
    integer                                             { $1 }
  | "-" integer                                         { context $1 <@@ (negate <\$> $2) }

integer :: { Contextual Integer }:
    integer_token                                       { integerTokenValue <\$> $1 }

string :: { Contextual ByteString }:
    string_token                                        { stringTokenValue <\$> $1 }
  | string string_token                                 { ByteString.append (value $1) (stringTokenValue $ value $2) :@@ between $1 $2 }

{

newtype Grammar a = G { unwrap :: G a }
type G a = Tokens -> List Error -> (Maybe a, List Error)
type Error = Positioned String

instance Monad Grammar where
  m >>= k = G $ bindG m k
  return x = G $ \ ts es -> (Just x, es)

bindG :: Grammar a -> (a -> Grammar b) -> G b
bindG m k ts es = (my, es1)
 where
  (mx, es1) = unwrap m ts es2
  (my, es2) = maybe (Nothing, es) (\x -> unwrap (k x) ts es) mx

lexer :: (Either Position (Contextual Token) -> Grammar a) -> Grammar a
lexer k = G act
 where
  act (t ::> ts) es = unwrap (k $ Right t) ts es
  act (e ::! ts) es = let (mx, es') = act ts es in (mx, e:es')
  act eof@(END_OF_TOKENS p) es = unwrap (k (Left p)) eof es

panic :: Error -> Grammar a
panic e = G $ \ ts es -> (Nothing, [e])

syntaxError :: Either Position (Contextual Token) -> Grammar a
syntaxError t = panic ("Syntax error" :@ either position position t)

parseWith :: Grammar a -> Tokens -> Either (List Error) a
parseWith g ts = maybe (Left es) (\r -> if null es then Right r else Left es) mr
 where (mr, es) = unwrap g ts []

checkSyntax :: Contextual ByteString -> Grammar ()
checkSyntax (x :@@ c)
  | (x == fromString "proto2") = return ()
  | otherwise = panic (msg :@ position c)
 where
  msg = concat ["Unsupported syntax requested: ", UTF8.toString $ quotedString x, " (expecting \"proto2\")"]

}
