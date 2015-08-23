{-|
See the documentation of 're'.
-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.Regex.TDFA.QuasiQuoter
( re
, unescape
) where

import Prelude
  ( Maybe(..)
  , Either(..)
  , String
  , (++)
  , all
  , otherwise
  , error
  , const
  , show
  )
--

import Numeric
  ( readHex
  )
--

import Data.Char
  ( isHexDigit
  , chr
  )
--

import Language.Haskell.TH.Quote
  ( QuasiQuoter(..)
  , dataToExpQ
  )
--

import Text.Regex.TDFA.ReadRegex
  ( parseRegex
  )
--

import Text.Regex.TDFA.Pattern
  ( Pattern(..)
  , PatternSet(..)
  , PatternSetCharacterClass(..)
  , PatternSetCollatingElement(..)
  , PatternSetEquivalenceClass(..)
  )
--

import Text.Regex.TDFA.Common
  ( DoPa(..)
  )
--

import Data.Typeable
  ( Typeable
  )
--

import Data.Data
  ( Data
  )
--

deriving instance Typeable PatternSetEquivalenceClass
deriving instance Data PatternSetEquivalenceClass

deriving instance Typeable PatternSetCollatingElement
deriving instance Data PatternSetCollatingElement

deriving instance Typeable PatternSetCharacterClass
deriving instance Data PatternSetCharacterClass

deriving instance Typeable DoPa
deriving instance Data DoPa

deriving instance Typeable PatternSet
deriving instance Data PatternSet

deriving instance Typeable Pattern
deriving instance Data Pattern

-- | Quasi-quoter for "Text.Regex.TDFA" (extended POSIX) regular
--   expressions. Refer to that module's documentation for more
--   information on supported features.
--
--   See the documentation of 'unescape' for escape sequences this
--   quasi-quoter supports.
--
--   @[re|regexp]@ is the parsed 'Text.Regex.TDFA.Pattern.Pattern' (AST)
--   of @regexp@. A parse failure is a compile-time error.
--
re :: QuasiQuoter
re = QuasiQuoter { quoteExp  = quoter
                 , quotePat  = error "no quotePat"
                 , quoteType = error "no quoteType"
                 , quoteDec  = error "no quoteDec"
                 }
  where
  quoter txt = dataToExpQ (const Nothing) pat
    where
    pat = case parseRegex (unescape txt) of
            Right pat -> pat
            Left  err -> error (show err)
--

-- | Replaces escape sequences with their respective characters. Any
--   sequence not listed will be left as-is.
--
-- @
-- Sequence  | Character
-- ----------+--------------------
-- \\\\      | \\
-- \\n       | Newline
-- \\r       | Carriage return
-- \\t       | Horizontal tab
-- \\f       | Form feed
-- \\v       | Vertical tab
-- \\xFFFF   | Code point (in hex)
-- |~]       | |]
-- \\|~]     | |~]
-- @
--
-- Note that if you are reading the source file and not the generated
-- Haddock documentation that the backslashes have been doubled up.
--
unescape :: String -> String
unescape = unescaped
  where
  
  delim ('|':'~':']':xs) = Just ("|]", xs)
  delim _                = Nothing
  
  control xxs@(d1:d2:d3:d4:xs)
    | all isHexDigit ds = Just ([chr v], xs)
    | otherwise         = Nothing
    where ds = [d1,d2,d3,d4]
          (v,_):_ = readHex ds
  control _ = Nothing
  
  escaped ('\\':xs) = Just ("\\", xs)
  escaped ('n' :xs) = Just ("\n", xs)
  escaped ('r' :xs) = Just ("\r", xs)
  escaped ('t' :xs) = Just ("\t", xs)
  escaped ('f' :xs) = Just ("\f", xs)
  escaped ('v' :xs) = Just ("\v", xs)
  escaped ('x' :xs) = control xs
  escaped ('|':'~':']':xs) = Just ("|~]", xs)
  escaped _         = Nothing
  
  unescaped ('\\':xs)  = case escaped xs of
                           Just (cs, xs') -> cs ++ unescaped xs'
                           Nothing        -> '\\' : unescaped xs
  unescaped xxs@(x:xs) = case delim xxs of
                           Just (cs, xs') -> cs ++ unescaped xs'
                           Nothing        -> x : unescaped xs
  unescaped []         = []
--