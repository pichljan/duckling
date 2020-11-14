-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

--tohle jsem pridal
{-# LANGUAGE LambdaCase #-}


module Duckling.Numeral.CS.Rules
  ( rules,
  )
where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import Duckling.Types
import Prelude

ruleIntersectWithAnd :: Rule
ruleIntersectWithAnd =
  Rule
    { name = "intersect (with and)",
      pattern =
        [ Predicate hasGrain,
          regex "a",
          Predicate $ and . sequence [not . isMultipliable, isPositive]
        ],
      prod = \case
        ( Token Numeral NumeralData {TNumeral.value = val1, TNumeral.grain = Just g}
            : _
            : Token Numeral NumeralData {TNumeral.value = val2}
            : _
          ) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
        _ -> Nothing
    }

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus =
  Rule
    { name = "numbers prefix with -, negative or minus",
      pattern =
        [ regex "-|mínus|záporné",
          Predicate isPositive
        ],
      prod = \case
        ( _
            : Token Numeral NumeralData {TNumeral.value = v}
            : _
          ) -> double $ v * (-1)
        _ -> Nothing
    }

ruleFew :: Rule
ruleFew =
  Rule
    { name = "few",
      pattern =
        [ regex "pár"
        ],
      prod = \_ -> integer 3
    }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator =
  Rule
    { name = "decimal with thousands separator",
      pattern =
        [ regex "(\\d+(\\.\\d\\d\\d)+\\,\\d+)"
        ],
      prod = \case
        ( Token RegexMatch (GroupMatch (match : _))
            : _
          ) ->
            let fmt = Text.replace "," "." $ Text.replace "." Text.empty match
             in parseDouble fmt >>= double
        _ -> Nothing
    }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral =
  Rule
    { name = "decimal number",
      pattern =
        [ regex "(\\d*,\\d+)"
        ],
      prod = \case
        ( Token RegexMatch (GroupMatch (match : _))
            : _
          ) -> parseDecimal False match
        _ -> Nothing
    }

ruleIntegerCompositeTens :: Rule
ruleIntegerCompositeTens =
  Rule
    { name = "integer 21..99",
      pattern =
        [ oneOf [20, 30 .. 90],
          numberBetween 1 10
        ],
      prod = \case
        ( Token Numeral NumeralData {TNumeral.value = tens}
            : Token Numeral NumeralData {TNumeral.value = units}
            : _
          ) -> double $ tens + units
        _ -> Nothing
    }

ruleSingle :: Rule
ruleSingle =
  Rule
    { name = "single",
      pattern =
        [ regex "jed(en|no|na)"
        ],
      prod = \_ -> integer 1 >>= withGrain 1
    }

ruleSum :: Rule
ruleSum =
  Rule
    { name = "intersect",
      pattern =
        [ Predicate hasGrain,
          Predicate $ and . sequence [not . isMultipliable, isPositive]
        ],
      prod = \case
        ( Token Numeral NumeralData {TNumeral.value = val1, TNumeral.grain = Just g}
            : Token Numeral NumeralData {TNumeral.value = val2}
            : _
          ) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
        _ -> Nothing
    }

ruleMultiply :: Rule
ruleMultiply =
  Rule
    { name = "compose by multiplication",
      pattern =
        [ dimension Numeral,
          Predicate isMultipliable
        ],
      prod = \case
        (token1 : token2 : _) -> multiply token1 token2
        _ -> Nothing
    }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG =
  Rule
    { name = "numbers suffixes (K, M, G)",
      pattern =
        [ dimension Numeral,
          regex "([kmg])(?=[\\W\\$€]|$)"
        ],
      prod = \case
        ( Token Numeral NumeralData {TNumeral.value = v}
            : Token RegexMatch (GroupMatch (match : _))
            : _
          ) -> case Text.toLower match of
            "k" -> double $ v * 1e3
            "m" -> double $ v * 1e6
            "g" -> double $ v * 1e9
            _ -> Nothing
        _ -> Nothing
    }

rulePowersOfTen :: Rule
rulePowersOfTen =
  Rule
    { name = "powers of tens",
      pattern =
        [ regex "(sto(vky)?|tisíce?|milióny?|miliardy?)"
        ],
      prod = \case
        (Token RegexMatch (GroupMatch (match : _)) : _) -> case Text.toLower match of
          "sto" -> double 1e2 >>= withGrain 2 >>= withMultipliable
          "stovky" -> double 1e2 >>= withGrain 2 >>= withMultipliable
          "tisíc" -> double 1e3 >>= withGrain 3 >>= withMultipliable
          "tisíce" -> double 1e3 >>= withGrain 3 >>= withMultipliable
          "milión" -> double 1e6 >>= withGrain 6 >>= withMultipliable
          "milióny" -> double 1e6 >>= withGrain 6 >>= withMultipliable
          "miliardy" -> double 1e9 >>= withGrain 9 >>= withMultipliable
          "miliarda" -> double 1e9 >>= withGrain 9 >>= withMultipliable
          _ -> Nothing
        _ -> Nothing
    }

ruleCouple :: Rule
ruleCouple =
  Rule
    { name = "couple, a pair",
      pattern =
        [ regex "páry?"
        ],
      prod = \_ -> integer 2
    }

ruleDozen :: Rule
ruleDozen =
  Rule
    { name = "dozen",
      pattern =
        [ regex "tucet"
        ],
      prod = \_ -> integer 12 >>= withGrain 1 >>= withMultipliable
    }

zeroNineMap :: HashMap Text Integer
zeroNineMap =
  HashMap.fromList
    [ ("nula", 0),
      ("jeden", 1),
      ("jedna", 1),
      ("jedno", 1),
      ("dva", 2),
      ("dvě", 2),
      ("tři", 3),
      ("čtyři", 4),
      ("pět", 5),
      ("šest", 6),
      ("sedm", 7),
      ("osm", 8),
      ("devět", 9)
    ]

rulesToNine :: Rule
rulesToNine =
  Rule
    { name = "number (0..9)",
      pattern =
        [ regex "(nula|jed(en|n[ao])|dv(a|ě)|tři|čtyři|pět|šest|sedm|osm|devět)"
        ],
      prod = \tokens -> case tokens of
        (Token RegexMatch (GroupMatch (match : _)) : _) ->
          HashMap.lookup (Text.toLower match) zeroNineMap >>= integer
        _ -> Nothing
    }

elevenNineteenMap :: HashMap Text Integer
elevenNineteenMap =
  HashMap.fromList
    [ ("jedenáct", 11),
      ("dvanáct", 12),
      ("třináct", 13),
      ("čtrnáct", 14),
      ("patnáct", 15),
      ("šestnáct", 16),
      ("sedmnáct", 17),
      ("osmnáct", 18),
      ("devatenáct", 19)
    ]

rulesElevenToNineteen :: Rule
rulesElevenToNineteen =
  Rule
    { name = "number (11..19)",
      pattern =
        [ regex "(jedenáct|dvanáct|třináct|čtrnáct|patnáct|šestnáct|sedmnáct|osmnáct|devatenáct)"
        ],
      prod = \tokens -> case tokens of
        (Token RegexMatch (GroupMatch (match : _)) : _) ->
          HashMap.lookup (Text.toLower match) elevenNineteenMap >>= integer
        _ -> Nothing
    }

tensMap :: HashMap Text Integer
tensMap =
  HashMap.fromList
    [ ("deset", 10),
      ("dvacet", 20),
      ("třicet", 30),
      ("čtyřicet", 40),
      ("padesát", 50),
      ("šedesát", 60),
      ("sedmdesát", 70),
      ("osmndesát", 80),
      ("devadesát", 90)
    ]

ruleTens :: Rule
ruleTens =
  Rule
    { name = "number (10..90)",
      pattern =
        [ regex "(deset|dvacet|třicet|čtyřicet|padesát|šedesát|sedmdesát|osmndesát|devadesát)"
        ],
      prod = \tokens -> case tokens of
        (Token RegexMatch (GroupMatch (match : _)) : _) ->
          HashMap.lookup (Text.toLower match) tensMap >>= integer
        _ -> Nothing
    }

hundredsMap :: HashMap Text Integer
hundredsMap =
  HashMap.fromList
    [ ("sto", 100),
      ("dvěstě", 200),
      ("třista", 300),
      ("čtyřista", 400),
      ("pětset", 500),
      ("šestset", 600),
      ("sedmset", 700),
      ("osmset", 800),
      ("dvěstě", 900)
    ]

ruleHundreds :: Rule
ruleHundreds =
  Rule
    { name = "number (100..900)",
      pattern =
        [ regex "(sto|dvěstě|třista|čtyřista|pětset|šestset|sedmset|osmset|dvěstě)"
        ],
      prod = \tokens -> case tokens of
        (Token RegexMatch (GroupMatch (match : _)) : _) ->
          HashMap.lookup (Text.toLower match) tensMap >>= integer
        _ -> Nothing
    }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral =
  Rule
    { name = "number dot number",
      pattern =
        [ dimension Numeral,
          regex "celá|celých|celé",
          Predicate $ not . hasGrain
        ],
      prod = \case
        (Token Numeral nd1 : _ : Token Numeral nd2 : _) ->
          double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
        _ -> Nothing
    }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator =
  Rule
    { name = "integer with thousands separator .",
      pattern =
        [ regex "(\\d{1,3}(\\.\\d\\d\\d){1,5})"
        ],
      prod = \case
        ( Token RegexMatch (GroupMatch (match : _))
            : _
          ) ->
            let fmt = Text.replace "." Text.empty match
             in parseDouble fmt >>= double
        _ -> Nothing
    }

rules :: [Rule]
rules =
  [ ruleCouple,
    ruleDecimalNumeral,
    ruleDecimalWithThousandsSeparator,
    ruleDozen,
    ruleFew,
    rulesToNine,
    rulesElevenToNineteen,
    ruleIntegerCompositeTens,
    ruleIntegerWithThousandsSeparator,
    ruleSum,
    ruleIntersectWithAnd,
    ruleMultiply,
    ruleNumeralDotNumeral,
    ruleNumeralsPrefixWithNegativeOrMinus,
    ruleNumeralsSuffixesKMG,
    rulePowersOfTen,
    ruleSingle
  ]