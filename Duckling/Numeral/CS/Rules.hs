-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
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

zeroNineMap :: HashMap Text Integer
zeroNineMap = HashMap.fromList
  [ ( "nula", 0 )
  , ( "jeden", 1 )
  , ( "jedna", 1 )
  , ( "jedno", 1 )
  , ( "dva", 2 )
  , ( "dvě", 2 )
  , ( "tři", 3 )
  , ( "čtyři", 4 )
  , ( "pět", 5)
  , ( "šest", 6)
  , ( "sedm", 7)
  , ( "osm", 8)
  , ( "devět", 9)
  ]

rulesToNine :: Rule
rulesToNine = Rule
  { name = "number (0..9)"
  , pattern =
    [ regex "(nula|jed(en|n[ao])|dv(a|ě)|tři|čtyři|pět|šest|sedm|osm|devět)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroNineMap >>= integer
      _ -> Nothing
  }

elevenNineteenMap :: HashMap Text Integer
elevenNineteenMap = HashMap.fromList
  [ ( "jedenáct"       , 11)
  , ( "dvanáct"        , 12)
  , ( "třináct"        , 13)
  , ( "čtrnáct"        , 14)
  , ( "patnáct"        , 15)
  , ( "šestnáct"       , 16)
  , ( "sedmnáct"       , 17)
  , ( "osmnáct"        , 18)
  , ( "devatenáct"     , 19)
  ]

rulesElevenToNineteen :: Rule
rulesElevenToNineteen = Rule
  { name = "number (11..19)"
  , pattern =
    [ regex "(jedenáct|dvanáct|třináct|čtrnáct|patnáct|šestnáct|sedmnáct|osmnáct|devatenáct)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) elevenNineteenMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "deset"                 , 10)
  , ( "dvacet"                , 20)
  , ( "třicet"                , 30)
  , ( "čtyřicet"              , 40)
  , ( "padesát"               , 50)
  , ( "šedesát"               , 60)
  , ( "sedmdesát"             , 70)
  , ( "osmndesát"             , 80)
  , ( "devadesát"             , 90)
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "number (10..90)"
  , pattern =
    [ regex "(deset|dvacet|třicet|čtyřicet|padesát|šedesát|sedmdesát|osmndesát|devadesát)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }



rules :: [Rule]
rules =
  [ rulesToNine
  , rulesElevenToNineteen
  , ruleTens
  ]
