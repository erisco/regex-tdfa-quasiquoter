{-| Internal definitions for "Text.Regex.TDFA.QuasiQuoter". Expect these
definitions to change.
-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.Regex.TDFA.QuasiQuoter.Internal where

import Prelude
  ( Either(..)
  , String(..)
  , fmap
  , foldr
  , (.)
  , (++)
  , null
  , Char
  , snd
  , fst
  , Maybe(..)
  , Bool(..)
  , not
  , maybe
  , either
  , error
  , Show(..)
  , Eq(..)
  , Ord(..)
  , all
  , otherwise
  )
--

import Control.Applicative
  ( (<|>)
  )
--

import Data.List
  ( find
  , intersect
  , nub
  , (\\)
  )
--

import qualified Text.Regex.TDFA.Common as TDFA
  ( CompOption(..)
  , ExecOption(..)
  )
--

import Text.Regex.TDFA.Common
  ( DoPa(..)
  , CompOption(CompOption)
  , ExecOption(ExecOption)
  , GroupIndex(..)
  )
--

import Data.Either
  ( partitionEithers
  )
--

import Control.Monad
  ( guard
  , Monad(..)
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

import Text.Regex.TDFA.TDFA
  ( patternToRegex
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

import Data.Char
  ( isHexDigit
  , chr
  )
--

import Numeric
  ( readHex
  )
--


-- | A flag indicates whether a particular option should be True ("on")
-- or False ("off") when compiling a regular expression string.
--
-- Refer to 'Text.Regex.TDFA.Common.CompOption' and
-- 'Text.Regex.TDFA.Common.ExecOption' for the meanings of each flag.
--
data Flag =
    CaseSensitive  { flagValue :: Bool }
  | Multiline      { flagValue :: Bool }
  | RightAssoc     { flagValue :: Bool }
  | NewSyntax      { flagValue :: Bool }
  | LastStarGreedy { flagValue :: Bool }
  | CaptureGroups  { flagValue :: Bool }
  deriving (Show, Eq, Ord)
--

-- | Options to use when compiling a regular expression string.
--
-- Refer to 'Text.Regex.TDFA.Common.CompOption' and
-- 'Text.Regex.TDFA.Common.ExecOption' for the meanings of each option.
--
data Options =
    Options { caseSensitive  :: Bool
            , multiline      :: Bool
            , rightAssoc     :: Bool
            , newSyntax      :: Bool
            , lastStarGreedy :: Bool
            , captureGroups  :: Bool
            }
  deriving (Show, Eq, Ord)
--

-- These standalone orphan instances are required for uses of
-- 'dataToExpQ' within this module.

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

deriving instance Typeable CompOption
deriving instance Data CompOption

deriving instance Typeable ExecOption
deriving instance Data ExecOption

-- | Replaces escape sequences with their respective characters. Any
--   sequence not listed will be left as-is.
--
-- @
-- Sequence  | Character
-- ----------+--------------------
-- \\\\        | \\
-- \\n        | Newline
-- \\r        | Carriage return
-- \\t        | Horizontal tab
-- \\f        | Form feed
-- \\v        | Vertical tab
-- \\xFFFF    | Code point (in hex)
-- |~]       | |]
-- \\|~]      | |~]
-- @
--
-- Note that if you are reading the source file and not the generated
-- Haddock documentation that the backslashes have been doubled up. This
-- is also why the table is incorrectly formatted in source.
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

-- | The AST for the regular expression string. If there is a parse
-- error a corresponding error string is returned.
--
compilePattern :: String -> Either String (Pattern, (GroupIndex, DoPa))
compilePattern = either (Left . show) Right . parseRegex . unescape

-- | The default options used when compiling a regular expression
-- string.
--
-- Refer to 'Text.Regex.TDFA.Common.CompOption' and
-- 'Text.Regex.TDFA.Common.ExecOption' for the meanings of each option.
--
defaultOptions :: Options
defaultOptions =
  Options { caseSensitive  = True
          , multiline      = True
          , rightAssoc     = True
          , newSyntax      = True
          , lastStarGreedy = False
          , captureGroups  = True
          }
--

-- | Converts this module's representation of regular expression string
-- compiler options to that of "Text.Regex.TDFA"'s.
--
optionsToTDFAOptions :: Options -> (CompOption, ExecOption)
optionsToTDFAOptions opts =
  ( CompOption { TDFA.caseSensitive  = caseSensitive  opts
               , TDFA.multiline      = multiline      opts
               , TDFA.rightAssoc     = rightAssoc     opts
               , TDFA.newSyntax      = newSyntax      opts
               , TDFA.lastStarGreedy = lastStarGreedy opts
               
               }
  , ExecOption { TDFA.captureGroups  = captureGroups  opts
               }
  )
--

-- | Overrides the option indicated by the flag.
--
applyFlag :: Flag -> Options -> Options
applyFlag (CaseSensitive  v) opts = opts { caseSensitive  = v }
applyFlag (Multiline      v) opts = opts { multiline      = v }
applyFlag (RightAssoc     v) opts = opts { rightAssoc     = v }
applyFlag (NewSyntax      v) opts = opts { newSyntax      = v }
applyFlag (LastStarGreedy v) opts = opts { lastStarGreedy = v }
applyFlag (CaptureGroups  v) opts = opts { captureGroups  = v }

-- | A relation of flags and characters. Each flag is related with one
-- character and that character is unique. Not all characters are
-- related to a flag.
--
flagsChars :: [(Char, Flag)]
flagsChars =
  [ ('C', CaseSensitive  True), ('c', CaseSensitive  False)
  , ('M', Multiline      True), ('m', Multiline      False)
  , ('R', RightAssoc     True), ('r', RightAssoc     False)
  , ('N', NewSyntax      True), ('n', NewSyntax      False)
  , ('A', LastStarGreedy True), ('a', LastStarGreedy False)
  , ('G', CaptureGroups  True), ('g', CaptureGroups  False)
  ]
--

-- | The flag related to the character (by 'flagsChars'), if any.
--
charToFlag :: Char -> Maybe Flag
charToFlag c = fmap snd (find ((==c) . fst) flagsChars)

-- | The character related to the flag (by 'flagsChars').
--
flagToChar :: Flag -> Char
flagToChar f =
  case find ((==f) . snd) flagsChars of
    Just (c,_) -> c
    Nothing    -> error ("Missing char for flag: " ++ show f)
--

-- | The flag which indicates the opposite option. That is, if the flag
-- indicates an option to be True ("on") then the negated flag indicates
-- the same option to be False ("off") and vice versa.
--
negateFlag :: Flag -> Flag
negateFlag f = f { flagValue = not (flagValue f) }

-- | The set of flags related to the set of characters using the
-- relation 'flagsChars'.
--
-- - No character may occur twice.
-- - If a character is present the character related to the negated flag
--   must not be.
-- - All characters must be related to a flag.
--
-- If one of these conditions are not met then a corresponding error
-- message is returned instead.
--
parseFlags :: String -> Either String [Flag]
parseFlags chars = do
  let results = fmap (\x -> maybe (Left x) Right (charToFlag x)) chars
      (notFlags, flags) = partitionEithers results
      extraFlags = flags \\ nub flags
      conflictingFlags = intersect flags (fmap negateFlag flags)
  guard (null notFlags)
    <|> Left ("These are not flags '" ++ notFlags ++ "'")
  guard (null extraFlags)
    <|> Left ("Duplicate flags '" ++ fmap flagToChar extraFlags ++ "'")
  guard (null conflictingFlags)
    <|> Left ("Conflicting flags '" ++ fmap flagToChar conflictingFlags
             ++ "'")
  Right flags
--

-- | The switch of Left and Right for an Either value.
--
switch :: Either a b -> Either b a
switch = either Right Left

-- | Apply all flags to the default options. See 'parseFlags' and
-- 'applyFlag'.
--
flagStringToOptions :: String -> Either String Options
flagStringToOptions = fmap (foldr applyFlag defaultOptions) . parseFlags
