-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.CS.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleIntersectWithAnd :: Rule
ruleIntersectWithAnd = Rule
  { name = "intersect (with and)"
  , pattern =
    [ Predicate hasGrain
    , regex "a"
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       _:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|mínus|záporné"
    , Predicate isPositive
    ]
  , prod = \case
      (_:
       Token Numeral NumeralData{TNumeral.value = v}:
       _) -> double $ v * (-1)
      _ -> Nothing
  }

ruleFew :: Rule
ruleFew = Rule
  { name = "few"
  , pattern =
    [ regex "pár"
    ]
  , prod = \_ -> integer 3
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+\\,\\d+)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace "," "." $ Text.replace "." Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*,\\d+)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> parseDecimal False match
      _ -> Nothing
  }

ruleIntegerCompositeTens :: Rule
ruleIntegerCompositeTens = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [20, 30..90]
    , numberBetween 1 10
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = tens}:
       Token Numeral NumeralData{TNumeral.value = units}:
       _) -> double $ tens + units
      _ -> Nothing
  }

ruleSingle :: Rule
ruleSingle = Rule
  { name = "single"
  , pattern =
    [ regex "jed(en|no|na)"
    ]
  , prod = \_ -> integer 1 >>= withGrain 1
  }

ruleSum :: Rule
ruleSum = Rule
  { name = "intersect"
  , pattern =
    [ Predicate hasGrain
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension Numeral
    , Predicate isMultipliable
    ]
  , prod = \case
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmg])(?=[\\W\\$€]|$)"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "k" -> double $ v * 1e3
         "m" -> double $ v * 1e6
         "g" -> double $ v * 1e9
         _   -> Nothing
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(sto(vky)?|set|tisíc.?|milion.?|miliard)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "sto"     -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "sty"     -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "set"     -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "stovky"  -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "tisíc"   -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "tisíce"  -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "milion"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milionů"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "miliony" -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "miliard" -> double 1e9 >>= withGrain 9 >>= withMultipliable
        _         -> Nothing
      _ -> Nothing
  }

ruleCouple :: Rule
ruleCouple = Rule
  { name = "couple, a pair"
  , pattern =
    [ regex "páry?"
    ]
  , prod = \_ -> integer 2
  }

ruleDozen :: Rule
ruleDozen = Rule
  { name = "dozen"
  , pattern =
    [ regex "tucet"
    ]
  , prod = \_ -> integer 12 >>= withGrain 1 >>= withMultipliable
  }

zeroToNineteenMap :: HashMap Text Integer
zeroToNineteenMap = HashMap.fromList
  [ ( "nula"    , 0 )
  , ( "jeden"   , 1 )
  , ( "jedna"   , 1 )
  , ( "jedno"   , 1 )
  , ( "dva"     , 2 )
  , ( "dvě"     , 2 )
  , ( "tři"     , 3 )
  , ( "čtyři"   , 4 )
  , ( "pět"     , 5 )
  , ( "šest"    , 6 )
  , ( "sedm"   , 7 )
  , ( "osm"    , 8 )
  , ( "devět"   , 9 )
  , ( "deset"   , 10 )
  , ( "jedenáct", 11 )
  , ( "dvanáct" , 12 )
  , ( "třináct" , 13 )
  , ( "čtrnáct" , 14 )
  , ( "patnáct" , 15 )
  , ( "šestnáct", 16 )
  , ( "sedmnáct", 17 )
  , ( "osmnáct" , 18 )
  , ( "devatenáct", 19 )
  ]

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..19)"
  -- e.g. jedenásť must be before jeden, otherwise jeden will always shadow jedenásť
  , pattern =
    [ regex "(nula|jed(enáct|en|na|no)|dv(anáct|a|ě)|třináct|tři|čtrnáct|čtyři|patnáct|pět|šestnáct|šest|sedmnáct|sedm|osmnáct|osm|devatenáct|devět|deset)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroToNineteenMap >>= integer
      _ -> Nothing
  }

dozenMap :: HashMap Text Integer
dozenMap = HashMap.fromList
  [ ( "dvaceti"        , 20)
  , ( "třiceti"        , 30)
  , ( "čtyřiceti"      , 40)
  , ( "padesáti"      , 50)
  , ( "šedesáti"     , 60)
  , ( "sedmdesáti"    , 70)
  , ( "osmdesáti"     , 80)
  , ( "devadesáti"    , 90)
  ]

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "((dva|tři|čtyri)ceti|(pa|še|sedm|osm|deva)desáti)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) dozenMap >>= integer
      _ -> Nothing
  }

dozenMap2 :: HashMap Text Integer
dozenMap2 = HashMap.fromList
  [ ( "dvacet"        , 20)
  , ( "třicet"        , 30)
  , ( "čtyřicet"      , 40)
  , ( "padesát"      , 50)
  , ( "šedesát"     , 60)
  , ( "sedmdesát"    , 70)
  , ( "osmdesát"     , 80)
  , ( "devadesát"    , 90)
  ]

ruleInteger5 :: Rule
ruleInteger5 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "((dva|tři|čtyri)cet|(pa|še|sedm|osm|deva)desát)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) dozenMap2 >>= integer
      _ -> Nothing
  }

zeroToNineteenMap3 :: HashMap Text Integer
zeroToNineteenMap3 = HashMap.fromList
  [ ( "nulou"    , 0 )
  , ( "jedním"   , 1 )
  , ( "jednou"   , 1 )
  , ( "dvěma"   , 2 )
  , ( "třemi"   , 3 )
  , ( "čtyřmi"     , 4 )
  , ( "pěti"     , 5 )
  , ( "šesti"     , 6 )
  , ( "sedmi"   , 7 )
  , ( "osmi"     , 8 )
  , ( "devíti"    , 9 )
  , ( "desíti"   , 10 )
  , ( "deseti"   , 10 )
  , ( "jednácti"    , 11 )
  , ( "dvanácti"   , 12 )
  , ( "třinácti"   , 13 )
  , ( "čtrnácti", 14 )
  , ( "patnácti" , 15 )
  , ( "šestnácti" , 16 )
  , ( "sedmnácti" , 17 )
  , ( "osmnácti" , 18 )
  , ( "devatenácti", 19 )
  , ( "dvaceti", 20 )
  ]

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer (0..19)"
  -- e.g. jedenásť must be before jeden, otherwise jeden will always shadow jedenásť
  , pattern =
    [ regex "(nulou|jedním|jednou|dvěma|třemi|čtyřmi|pěti|šesti|sedmi|osmi|devíti|deseti|desíti|jednácti|dvanácti|třinácti|čtrnácti|patnácti|šestnácti|sedmnácti|osmnácti|devatenácti|dvaceti)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroToNineteenMap3 >>= integer
      _ -> Nothing
  }

zeroToNineteenMap4 :: HashMap Text Integer
zeroToNineteenMap4 = HashMap.fromList
  [( "poloviny"   , 2 )
  , ( "třetiny"   , 3 )
  , ( "čtvrtiny"     , 4 )
  , ( "pětiny"     , 5 )
  , ( "šestiny"     , 6 )
  , ( "sedminy"   , 7 )
  , ( "osminy"     , 8 )
  , ( "devítiny"    , 9 )
  , ( "desítiny"   , 10 )
  , ( "jednáctiny"    , 11 )
  , ( "dvanáctiny"   , 12 )
  , ( "třináctiny"   , 13 )
  , ( "čtrnáctiny", 14 )
  , ( "patnáctiny" , 15 )
  , ( "šestnáctiny" , 16 )
  , ( "sedmnáctiny" , 17 )
  , ( "osmnáctiny" , 18 )
  , ( "devatenáctiny", 19 )
  , ( "dvacetiny", 20 )
  ]

ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer (0..19)"
  -- e.g. jedenásť must be before jeden, otherwise jeden will always shadow jedenásť
  , pattern =
    [ regex "(poloviny|(třeti|čtvrti|pěti|šesti|sedmi|osmi|devíti|desíti|jednácti|dvanácti|třinácti|čtrnácti|patnácti|šestnácti|sedmnácti|osmnácti|devatenácti|dvaceti)ny)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroToNineteenMap4 >>= integer
      _ -> Nothing
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "celá|celých|celé"
    , Predicate $ not . hasGrain
    ]
  , prod = \case
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ."
  , pattern =
    [ regex "(\\d{1,3}(\\.\\d\\d\\d){1,5})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace "." Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleCouple
  , ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleDozen
  , ruleFew
  , ruleInteger4
  , ruleInteger3
  , ruleInteger2
  , ruleInteger
  , ruleInteger5
  , ruleIntegerCompositeTens
  , ruleIntegerWithThousandsSeparator
  , ruleSum
  , ruleIntersectWithAnd
  , ruleMultiply
  , ruleNumeralDotNumeral
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  , rulePowersOfTen
  , ruleSingle
  ]
