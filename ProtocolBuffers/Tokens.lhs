Haskell Protocol Buffer Compiler - Lexical Tokens
=================================================

    Copyright © 2014 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

Data type used to describe individual lexical tokens, as well
as sequences of such tokens. A token sequence may include embedded
error messages, and always ends in a special "END_OF_TOKENS" element.
All tokens in a sequence are annotated with their source context,
and "END_OF_TOKENS" is annotated with position information.

> module ProtocolBuffers.Tokens (
>   Token (..), Tokens (..),
>   nameTokenValue,
>   integerTokenValue,
>   rationalTokenMantissa, rationalTokenExponent,
>   stringTokenValue,
>   tokenLexeme,
>   quotedString,
>   normalisedScientific
> ) where

> import Data.Bits
> import Data.ByteString (ByteString)
> import Data.Char
> import Data.List
> import Data.String
> import Data.Word
> import Numeric
> import ProtocolBuffers.Contexts
> import ProtocolBuffers.Positions
> import ProtocolBuffers.Utilities

> import qualified Data.ByteString as ByteString

> infixr 5 ::>, ::!

> data Token =

Identifier, integer, rational and string tokens,
with preprocessed values. Rationals are stored as
integer mantissa and decimal exponent, normalised
to minimise the mantissa's value, while strings are
stored as raw binary data, with quotes removed
and escape sequences replaced by their constituent
characters:

>     IDENTIFIER_TOKEN  !Name
>   | INTEGER_TOKEN     !Integer
>   | RATIONAL_TOKEN    !Integer !Integer
>   | STRING_TOKEN      !ByteString

Keywords, without lexeme attributes, which are
reconstructed easily enough. We distinguish them
from identifiers to simplify parsing:

>   | ENUM_TOKEN        | EXTEND_TOKEN      | EXTENSIONS_TOKEN
>   | IMPORT_TOKEN      | MAX_TOKEN         | MESSAGE_TOKEN
>   | ONEOF_TOKEN       | OPTION_TOKEN      | OPTIONAL_TOKEN
>   | PACKAGE_TOKEN     | PUBLIC_TOKEN      | REPEATED_TOKEN
>   | REQUIRED_TOKEN    | RETURNS_TOKEN     | RPC_TOKEN
>   | SERVICE_TOKEN     | SYNTAX_TOKEN      | TO_TOKEN
>   | WEAK_TOKEN

Same goes for the few symbol tokens:

>   | LBRACE_TOKEN      | RBRACE_TOKEN
>   | LBRACKET_TOKEN    | RBRACKET_TOKEN
>   | LPAREN_TOKEN      | RPAREN_TOKEN
>   | SEMICOLON_TOKEN   | COMMA_TOKEN
>   | PERIOD_TOKEN      | EQUALS_TOKEN
>   | MINUS_TOKEN

The "show" instance formats each token's canonical lexeme in
angle brackets, and formats a list of tokens as space-separated
sentence:

> instance Show Token where
>   showsPrec _ t = showChar '<' . showByteString (tokenLexeme t) . showChar '>'
>   showList = foldl (.) id . intersperse (showChar ' ') . map shows

A list of tokens annotated with context information, intermixed
with error messages and ending in an explicit "END_OF_TOKENS"
marker, used to represent the output of lexical analyser:

> data Tokens =
>     Contextual Token ::> Tokens
>   | Positioned String ::! Tokens
>   | END_OF_TOKENS !Position
>   deriving (Show)

Partial functions extracting token attributes:

> nameTokenValue (IDENTIFIER_TOKEN x) = x
> integerTokenValue (INTEGER_TOKEN x) = x
> rationalTokenMantissa (RATIONAL_TOKEN x e) = x
> rationalTokenExponent (RATIONAL_TOKEN x e) = e
> stringTokenValue (STRING_TOKEN x) = x

Extracts the canonical lexeme of every token; this isn't necessarily
identical to what appeared in the source file, but is semantically
equivalent. In particular, all integers end up in decimal, all
rationals end up in their minimal scientific notation with lower-case
exponent, and all strings end up in double-quote format with all
non-printable characters escaped using either symbolic or minimal
octal escape sequences.

> tokenLexeme :: Token -> ByteString
> tokenLexeme (IDENTIFIER_TOKEN x)  = x
> tokenLexeme (INTEGER_TOKEN x)     = fromString $ shows x $ ""
> tokenLexeme (RATIONAL_TOKEN m e)  = fromString $ uncurry showScientific (normalisedScientific m e) ""
> tokenLexeme (STRING_TOKEN x)      = quotedString x
> tokenLexeme (ENUM_TOKEN)          = fromString "enum"
> tokenLexeme (EXTEND_TOKEN)        = fromString "extend"
> tokenLexeme (EXTENSIONS_TOKEN)    = fromString "extensions"
> tokenLexeme (IMPORT_TOKEN)        = fromString "import"
> tokenLexeme (MAX_TOKEN)           = fromString "max"
> tokenLexeme (MESSAGE_TOKEN)       = fromString "message"
> tokenLexeme (ONEOF_TOKEN)         = fromString "oneof"
> tokenLexeme (OPTION_TOKEN)        = fromString "option"
> tokenLexeme (OPTIONAL_TOKEN)      = fromString "optional"
> tokenLexeme (PACKAGE_TOKEN)       = fromString "package"
> tokenLexeme (PUBLIC_TOKEN)        = fromString "public"
> tokenLexeme (REPEATED_TOKEN)      = fromString "repeated"
> tokenLexeme (REQUIRED_TOKEN)      = fromString "required"
> tokenLexeme (RETURNS_TOKEN)       = fromString "returns"
> tokenLexeme (RPC_TOKEN)           = fromString "rpc"
> tokenLexeme (SERVICE_TOKEN)       = fromString "service"
> tokenLexeme (SYNTAX_TOKEN)        = fromString "syntax"
> tokenLexeme (TO_TOKEN)            = fromString "to"
> tokenLexeme (WEAK_TOKEN)          = fromString "weak"
> tokenLexeme (LBRACE_TOKEN)        = fromString "{"
> tokenLexeme (RBRACE_TOKEN)        = fromString "}"
> tokenLexeme (LBRACKET_TOKEN)      = fromString "["
> tokenLexeme (RBRACKET_TOKEN)      = fromString "]"
> tokenLexeme (LPAREN_TOKEN)        = fromString "("
> tokenLexeme (RPAREN_TOKEN)        = fromString ")"
> tokenLexeme (SEMICOLON_TOKEN)     = fromString ";"
> tokenLexeme (COMMA_TOKEN)         = fromString ","
> tokenLexeme (PERIOD_TOKEN)        = fromString "."
> tokenLexeme (EQUALS_TOKEN)        = fromString "="
> tokenLexeme (MINUS_TOKEN)         = fromString "-"

Normalise a scientific mantissa and exponent:

> normalisedScientific :: Integer -> Integer -> (Integer, Integer)
> normalisedScientific 0 _ = (0, 0)
> normalisedScientific m e | (m < 0) = (negate m', e')
>  where (m', e') = normalisedScientific (negate m) e
> normalisedScientific m e | (m `mod` 10 == 0) = normalisedScientific (m `div` 10) (e + 1)
> normalisedScientific m e = (m, e)

Format a rational using standard scientific notation:

> showScientific :: Integer -> Integer -> ShowS
> showScientific m e
>   | m == 0 = showString "0.0E0"
>   | m < 0 = showChar '-' . showScientific (negate m) e
>   | otherwise = showChar is . showChar '.' . showString (if null fs then "0" else fs) . showChar 'E' . shows (e + fromIntegral (length fs))
>  where
>   (is:fs) = show m

Quote and escape string literals:

> quotedString :: ByteString -> ByteString
> quotedString x = ByteString.pack $ (quoteChar '\"' :) $ quoteBytes $ ByteString.unpack x
>  where
>   quoteBytes (0x07:bs) = quoteChar '\\' : quoteChar 'a'  : quoteBytes bs
>   quoteBytes (0x08:bs) = quoteChar '\\' : quoteChar 'b'  : quoteBytes bs
>   quoteBytes (0x09:bs) = quoteChar '\\' : quoteChar 't'  : quoteBytes bs
>   quoteBytes (0x0A:bs) = quoteChar '\\' : quoteChar 'n'  : quoteBytes bs
>   quoteBytes (0x0B:bs) = quoteChar '\\' : quoteChar 'v'  : quoteBytes bs
>   quoteBytes (0x0C:bs) = quoteChar '\\' : quoteChar 'f'  : quoteBytes bs
>   quoteBytes (0x0D:bs) = quoteChar '\\' : quoteChar 'r'  : quoteBytes bs
>   quoteBytes (0x22:bs) = quoteChar '\\' : quoteChar '\"' : quoteBytes bs
>   quoteBytes (0x5C:bs) = quoteChar '\\' : quoteChar '\\' : quoteBytes bs
>   quoteBytes (b:bs)
>     | (32 <= b && b <= 126) = b : quoteBytes bs
>     | otherwise = quoteChar '\\' : quoteOctal b bs
>   quoteBytes [] = [quoteChar '\"']
>
>   quoteOctal b bs
>     | (b > 0o77 || startsWithOctit bs) = quoteOctit (b `shiftR` 6) : quoteOctit ((b `shiftR` 3) .&. 7) : quoteOctit (b .&. 7) : quoteBytes bs
>     | (b > 0o7) = quoteOctit (b `shiftR` 3) : quoteOctit (b .&. 0o7) : quoteBytes bs
>     | otherwise = quoteOctit b : quoteBytes bs
>
>   startsWithOctit (b:bs) = quoteChar '0' <= b && b <= quoteChar '7'
>   startsWithOctit [] = False
>
>   quoteOctit :: Word8 -> Word8
>   quoteOctit x = x + quoteChar '0'
>
>   quoteChar :: Char -> Word8
>   quoteChar = fromIntegral . ord
