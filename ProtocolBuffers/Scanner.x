{

{-

Haskell Protocol Buffer Compiler - Lexical Analyser
===================================================

    Copyright © 2014 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

Alex scanner for .proto files, with minor generalisations.
The output is a sequence of tokens and error messages defined
in the "Tokens" module, annotated with source code context
defined in "Contexts".

-}

module ProtocolBuffers.Scanner (Tokens, scanTokens) where

import Data.ByteString (ByteString)
import Data.Char
import Data.Functor
import Data.Maybe
import Data.Word
import Numeric
import ProtocolBuffers.Contexts
import ProtocolBuffers.Positions
import ProtocolBuffers.Tokens
import ProtocolBuffers.Utilities

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as UTF8

}

$space                      = [ \ \t \v \f ]
$digit                      = [ 0-9 ]
$octit                      = [ 0-7 ]
$hexit                      = [ 0-9 a-f A-F ]
$alpha                      = [ a-z A-Z _ ]
$alnum                      = [ $alpha $digit ]
$sign                       = [ \+ \- ]
$print                      = [ \ - \~ ]

@identifier                 = $alpha $alnum*
@decimal_integer            = [ $digit # 0 ] $digit*
@octal_integer              = 0 $octit*
@hexadecimal_integer        = 0 [xX] $hexit+
@integer                    = @decimal_integer | @octal_integer | @hexadecimal_integer

@exponent                   = [eE] $sign? $digit+
@rational                   = $digit* \. $digit+ @exponent? | $digit+ @exponent

@space                      = $space+
@newline                    = \n
@line_comment               = "//" [^ \n ]* @newline?
@block_comment              = "/*" (("/"+ | "*"+)? ([^ \* \/ \n ] | @newline))* "*"* "*/"
@whitespace                 = (@space | @newline | @line_comment | @block_comment)+

tokens :-

<0> @whitespace             ;

<0> "{"                     { produceSymbol LBRACE_TOKEN }
<0> "}"                     { produceSymbol RBRACE_TOKEN }
<0> "["                     { produceSymbol LBRACKET_TOKEN }
<0> "]"                     { produceSymbol RBRACKET_TOKEN }
<0> "("                     { produceSymbol LPAREN_TOKEN }
<0> ")"                     { produceSymbol RPAREN_TOKEN }
<0> ";"                     { produceSymbol SEMICOLON_TOKEN }
<0> ","                     { produceSymbol COMMA_TOKEN }
<0> "."                     { produceSymbol PERIOD_TOKEN }
<0> "="                     { produceSymbol EQUALS_TOKEN }
<0> "-"                     { produceSymbol MINUS_TOKEN }

<0> "enum"                  { produceSymbol ENUM_TOKEN }
<0> "extend"                { produceSymbol EXTEND_TOKEN }
<0> "extensions"            { produceSymbol EXTENSIONS_TOKEN }
<0> "import"                { produceSymbol IMPORT_TOKEN }
<0> "max"                   { produceSymbol MAX_TOKEN }
<0> "message"               { produceSymbol MESSAGE_TOKEN }
<0> "oneof"                 { produceSymbol ONEOF_TOKEN }
<0> "option"                { produceSymbol OPTION_TOKEN }
<0> "optional"              { produceSymbol OPTIONAL_TOKEN }
<0> "package"               { produceSymbol PACKAGE_TOKEN }
<0> "public"                { produceSymbol PUBLIC_TOKEN }
<0> "repeated"              { produceSymbol REPEATED_TOKEN }
<0> "required"              { produceSymbol REQUIRED_TOKEN }
<0> "returns"               { produceSymbol RETURNS_TOKEN }
<0> "rpc"                   { produceSymbol RPC_TOKEN }
<0> "service"               { produceSymbol SERVICE_TOKEN }
<0> "syntax"                { produceSymbol SYNTAX_TOKEN }
<0> "to"                    { produceSymbol TO_TOKEN }
<0> "weak"                  { produceSymbol WEAK_TOKEN }

<0> @identifier             { produceToken IDENTIFIER_TOKEN $ ByteString.take }
<0> @decimal_integer        { produceToken INTEGER_TOKEN $ parseWith readDec }
<0> @octal_integer          { produceToken INTEGER_TOKEN $ parseWith readOct }
<0> @hexadecimal_integer    { produceToken INTEGER_TOKEN $ parseWith (readHex . drop 2) }
<0> @rational               { produceToken (uncurry RATIONAL_TOKEN) $ parseWith readRational }

<0> \"                      { beginString sq }
<0> \'                      { beginString sa }

<sq> [ $print # [\"\\] ]+   { produceStringPart }
<sa> [ $print # [\'\\] ]+   { produceStringPart }
<sq,sa> \\ a                { produceSimpleEscape '\a' }
<sq,sa> \\ b                { produceSimpleEscape '\b' }
<sq,sa> \\ f                { produceSimpleEscape '\f' }
<sq,sa> \\ n                { produceSimpleEscape '\n' }
<sq,sa> \\ r                { produceSimpleEscape '\r' }
<sq,sa> \\ t                { produceSimpleEscape '\t' }
<sq,sa> \\ v                { produceSimpleEscape '\v' }
<sq,sa> \\ \\               { produceSimpleEscape '\\' }
<sq,sa> \\ \'               { produceSimpleEscape '\'' }
<sq,sa> \\ \"               { produceSimpleEscape '\"' }
<sq,sa> \\ \?               { produceSimpleEscape '?'  }
<sq,sa> \\ $octit{1,3}      { produceOctalEscape }
<sq,sa> \\ [xX] $hexit+     { produceHexadecimalEscape }
<sq,sa> \\ u $hexit{4}      { produceUnicodeEscape }
<sq,sa> \\ U $hexit{8}      { produceUnicodeEscape }
<sq,sa> \\                  { reportIllegalEscape }
<sq,sa> @newline            { reportIncompleteStringLiteral }
<sq> \"                     { endString }
<sa> \'                     { endString }

{

-- Alex input is simply a positioned byte string:
type AlexInput = Positioned ByteString

-- For simplicity, we keep track of character positions before handing them off to Alex
-- and take the opportunity to normalise all line breaks including CR+LF sequences into LFs:
alexGetByte (bs :@ p) = do
  (x, bs') <- ByteString.uncons bs
  case x of
    0x0D -> case ByteString.uncons bs' of
              Just (0x0A, bs'') -> return (0x0A, bs'' :@ nextLine p)
              _ -> return (0x0A, bs' :@ nextLine p)
    0x0A -> return (x, bs' :@ nextLine p)
    0x09 -> return (x, bs' :@ nextTab p)
    _    -> return (x, bs' :@ nextColumn p)

-- Actual entry point into the scanner: parses a positioned bytestring into a list tokens:
scanTokens :: Positioned ByteString -> Tokens
scanTokens inp =
  case alexScan inp 0 of
    AlexToken inp' len act -> act undefined 0 inp len inp'
    AlexSkip inp' len      -> scanTokens inp'
    AlexError inp'         -> fmap unexpected inp ::! scanTokens inp'
    AlexEOF                -> END_OF_TOKENS $ position inp

-- Produce a human-readable error message:
unexpected :: ByteString -> String
unexpected bs = "Unexpected " ++ inputContext bs

-- Establish human-readable input context:
inputContext :: ByteString -> String
inputContext bs = fromMaybe "end of file" $ do
  (x, _) <- UTF8.uncons bs
  return ("character " ++ show x)

-- Produce a single token:
produceToken :: (a -> Token) -> (Int -> ByteString -> a) -> s -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
produceToken t p _ _ inp len inp' = (t $ p len $ value inp) :@@ between inp inp' ::> scanTokens inp'

-- Combinator that allows us to annotate a token with its actual value using a standard ReadS function:
parseWith :: ReadS a -> Int -> ByteString -> a
parseWith r len str = v
 where [(v, "")] = r $ UTF8.toString $ ByteString.take len str

-- Reader for rational token values:
readRational :: ReadS (Integer, Integer)
readRational s = readIntegerPart 0 s
 where
  readIntegerPart m (c:xs)
    | ('0' <= c && c <= '9') = readIntegerPart (m * 10 + digitValue c) xs
    | (c == '.') = readFractionPart m 0 xs
    | (c == 'e' || c == 'E') = readExponentPart m 0 xs
  readIntegerPart m xs = [(normalisedScientific m 0, xs)]
  readFractionPart m e (c:xs)
    | ('0' <= c && c <= '9') = readFractionPart (m * 10 + digitValue c) (e - 1) xs
    | (c == 'e' || c == 'E') = readExponentPart m e xs
  readFractionPart m e xs = [(normalisedScientific m e, xs)]
  readExponentPart m e ('-':xs) = readExponentValue m e (-1) 0 xs
  readExponentPart m e ('+':xs) = readExponentValue m e  (1) 0 xs
  readExponentPart m e     (xs) = readExponentValue m e  (1) 0 xs
  readExponentValue m e s e' (c:xs)
    | ('0' <= c && c <= '9') = readExponentValue m e s (e' * 10 + digitValue c) xs
  readExponentValue m e s e' xs = [(normalisedScientific m (e + s * e'), xs)]
  digitValue c = fromIntegral (ord c - ord '0')

-- Symbols are trivial, since they have no features other than their position position:
produceSymbol :: Token -> s -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
produceSymbol t _ _ inp len inp' = t :@@ between inp inp' ::> scanTokens inp'

-- String literals are a little bit tricky, since, in the interest of sensible error messages,
-- the scanner needs to process escape sequences into the string's ultimate form.
-- While the input is assumed to be UTF-8, the resulting string is always binary,
-- and all non-ASCII characters must be represented using octal or hexadecimal escape sequences.
-- Non-ASCII Unicode characters will, in the current version, result in errors since we don't
-- want to presume any specific encoding on the user's part.

beginString :: Int -> s -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
beginString sc _ _ inp len inp' = scanString ([] <$ inp) sc inp'

scanString :: Positioned [ByteString] -> Int -> Positioned ByteString -> Tokens
scanString rs sc inp =
  case alexScan inp sc of
    AlexToken inp' len act -> act rs sc inp len inp'
    AlexSkip inp' len      -> scanString rs sc inp'
    AlexError inp'         -> fmap unexpected inp ::! scanString rs sc inp'
    AlexEOF                -> reportIncompleteStringLiteral rs sc inp 0 inp

endString :: Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
endString rs sc inp len inp' = (STRING_TOKEN $ ByteString.concat $ reverse $ value rs) :@@ between rs inp' ::> scanTokens inp'

produceStringPart :: Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
produceStringPart rs sc inp len inp' = scanString (fmap (r :) rs) sc inp'
  where r = ByteString.take len (value inp)

produceSimpleEscape :: Char -> Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
produceSimpleEscape c rs sc inp len inp' = scanString (fmap (r :) rs) sc inp'
  where r = ByteString.singleton $ fromIntegral $ ord c

produceOctalEscape :: Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
produceOctalEscape rs sc inp len inp'
  | x <= 0xFF = scanString (fmap (r :) rs) sc inp'
  | otherwise = ("Illegal octal escape sequence; every byte in a string literal must have a value less than 256" <$ inp) ::! scanString rs sc inp'
 where
  x = parseWith (readOct . tail) len (value inp) :: Int
  r = ByteString.singleton $ fromIntegral x

produceHexadecimalEscape :: Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
produceHexadecimalEscape rs sc inp len inp'
  | x <= 0xFF = scanString (fmap (r :) rs) sc inp'
  | otherwise = ("Illegal hexadecimal escape sequence; every byte in a string literal must have a value less than 256" <$ inp) ::! scanString rs sc inp'
 where
  x = parseWith (readHex . drop 2) len (value inp) :: Integer
  r = ByteString.singleton $ fromIntegral x

produceUnicodeEscape :: Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
produceUnicodeEscape rs sc inp len inp'
  | x <= 0x7F = scanString (fmap (r :) rs) sc inp'
  | otherwise = ("Illegal Unicode escape sequence; only ASCII characters are supported in string literals" <$ inp) ::! scanString rs sc inp'
 where
  x = parseWith (readHex . drop 2) len (value inp) :: Word32
  r = ByteString.singleton $ fromIntegral x

reportIllegalEscape :: Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
reportIllegalEscape rs sc inp len inp' = ("Illegal escape sequence" <$ inp) ::! scanString rs sc inp'

reportIncompleteStringLiteral :: Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
reportIncompleteStringLiteral rs sc inp len inp' = ("Incomplete string literal" <$ inp) ::! scanTokens inp'

}
