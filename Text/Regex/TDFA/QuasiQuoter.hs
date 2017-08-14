{-| Quasi-quoters for "Text.Regex.TDFA" (extended POSIX) regular
expressions. Refer to that module's documentation for more
information on supported features.

Quasi-quoters included in this module are:

  - 're' For splicing the expression to compile a regular expression AST
    to a 'Text.Regex.TDFA.Common.Regex' value.
  - 'pat' For splicing a regular expression AST of type
    'Text.Regex.TDFA.Common.Pattern'.

-}
{-# LANGUAGE TemplateHaskell #-}
module Text.Regex.TDFA.QuasiQuoter where

import Prelude
  ( Either(..)
  , Maybe(..)
  , const
  , error
  , (++)
  , Eq(..)
  , break
  )
--

import Language.Haskell.TH.Quote
  ( QuasiQuoter(..)
  , dataToExpQ
  )
--

import Text.Regex.TDFA.TDFA
  ( patternToRegex
  )
--

import Text.Regex.TDFA.QuasiQuoter.Internal

-- | Quasi-quoter for "Text.Regex.TDFA" (extended POSIX) regular
-- expressions. Refer to that module's documentation for more
-- information on supported features.
--
-- See the documentation of 'unescape' for escape sequences this
-- quasi-quoter supports.
--
-- @[re|flags;regexp|]@ splices the expression to compile the AST
-- (of type 'Text.Regex.TDFA.Common.Pattern') of @regexp@ using the
-- 'Text.Regex.TDFA.Common.CompOption' and
-- 'Text.Regex.TDFA.Common.ExecOption' indicated by @flags@. Incorrect
-- flags or a failure to parse the regular expression is a
-- compile-time error.
--
-- Examples:
--
--   - @[re|ga;a+b+c+|]@ has @flags = MCgRaN@ and @regexp = a+b+c+@.
--
--   - @[re|;(z*)|]@ has @flags = MCGRaN@ and @regexp = (z*)@.
--
-- Every flag name is assigned a unique upper and lower case letter pair.
-- The uppercase letter indicates the flag to be True ("on"), and the
-- lowercase letter indicates the flag to be False ("off").
--
-- @
-- Flag name          |  True  |  False  |  Default
-- -------------------+--------+-------------------
-- Multiline          |  M     |  m      |  M
-- Case sensitive     |  C     |  c      |  C
-- Capture groups     |  G     |  g      |  G
-- Right associative  |  R     |  r      |  R
-- Last star greedy   |  A     |  a      |  a
-- New syntax         |  N     |  n      |  N
-- @
--
-- Refer to 'Text.Regex.TDFA.Common.CompOption' and
-- 'Text.Regex.TDFA.Common.ExecOption' for the meanings of each flag.
--
-- For the significance of associativity see
-- <https://wiki.haskell.org/Regex_Posix> Section 3 "Types of failure".
--
-- The default flags are 'MCGRaN'. Zero or more explicit flags must be
-- specified in the @flags@ field. Explicit flags take precedence over
-- default flags. Every explicit flag must occur at most once. If a
-- flag is explicit the negated flag must not also be.
--
re :: QuasiQuoter
re = QuasiQuoter { quoteExp  = quoter
                 , quotePat  = error "no quotePat"
                 , quoteType = error "no quoteType"
                 , quoteDec  = error "no quoteDec"
                 }
  where
  
  quoter txt = [e|patternToRegex $patternExp $compOptsExp $execOptsExp|]
    where
    
    (flagString, patternString) =
      case break (==';') txt of
        (flagString, ';':pat) -> (flagString, pat)
        _ -> error ("Regular expression format must be 'flags;regexp' "
                   ++ "in '" ++ txt ++ "'")
    pattern =
      case compilePattern patternString of
        Right pat -> pat
        Left  err -> error ("Error in regular expression '" ++ txt
                           ++ "': " ++ err)
    
    options =
      case flagStringToOptions flagString of
        Right opts -> opts
        Left  err  -> error ("Error in regular expression '" ++ txt
                            ++ "': " ++ err)
         
    (compOpts, execOpts) = optionsToTDFAOptions options
    patternExp = dataToExpQ (const Nothing) pattern
    compOptsExp = dataToExpQ (const Nothing) compOpts
    execOptsExp = dataToExpQ (const Nothing) execOpts
--

-- | Quasi-quoter for "Text.Regex.TDFA" (extended POSIX) regular
-- expressions. Refer to that module's documentation for more
-- information on supported features.
--
-- See the documentation of 'unescape' for escape sequences this
-- quasi-quoter supports.
--
-- @[pat|regexp]@ splices the AST (of type
-- 'Text.Regex.TDFA.Pattern.Pattern') of @regexp@. A parse failure is a
-- compile-time error.
--
pat :: QuasiQuoter
pat = QuasiQuoter { quoteExp  = quoter
                  , quotePat  = error "no quotePat"
                  , quoteType = error "no quoteType"
                  , quoteDec  = error "no quoteDec"
                  }
  where
  
  quoter txt = dataToExpQ (const Nothing) pattern
    where
    
    pattern =
      case compilePattern txt of
        Right pat -> pat
        Left  err -> error ("Error in regular expression '" ++ txt
                           ++ "': " ++ err)
--

