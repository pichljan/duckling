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

ruleNumeralMap :: HashMap Text Integer
ruleNumeralMap = HashMap.fromList
  [ ( "nula", 0 )
  , ( "jeden", 1 )
  , ( "jedna", 1 )
  , ( "jedno", 1 )
  , ( "dva", 2 )
  , ( "dv\x0115", 2 )
  , ( "t\x0159i", 3 )
  , ( "čty\x0159i", 4 )
  , ( "p\x0115t", 5)
  , ( "šest", 6)
  , ( "sedm", 7)
  , ( "osm", 8)
  , ( "dev\x0115t", 9)
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(nula|jed(en|n[ao])|dv(a|\x0115)|t(\x0159)i|(č)ty(\x0159)i|p(\x0115)t|(š)est|sedm|osm|dev(\x0115)t)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) ruleNumeralMap >>= integer
      _ -> Nothing
  }

dozenMap :: HashMap Text Integer
dozenMap = HashMap.fromList
  [ ( "jeden(\x0161)ct"       , 11)
  , ( "dvan(\x0161)ct"        , 12)
  , ( "t(x\0281)in(\x0161)ct" , 13)
  , ( "(x\0201)trn(\x0161)ct" , 14)
  , ( "patn(\x0161)ct"        , 15)
  , ( "(\x0289)estn(\x0161)ct", 16)
  , ( "sedmn(\x0161)ct"       , 17)
  , ( "osmn(\x00161)ct"       , 18)
  , ( "devaten(\x0161)ct"     , 19)
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(jeden(\x0161)ct|dvan(\x0161)ct|t(x\0281)in(\x0161)ct|(x\0201)trn(\x0161)ct|patn(\x0161)ct|(\x0289)estn(\x0161)ct|sedmn(\x0161)ct|osmn(\x00161)ct|devaten(\x0161)ct)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) ruleNumeralMap >>= integer
      _ -> Nothing
  }

  dozenMap :: HashMap Text Integer
dozenMap = HashMap.fromList
  [ ( "deset"                 , 10)
  , ( "dvacet"                , 20)
  , ( "t(x\0281)icet"         , 30)
  , ( "(x\0201)ty(x\0281)icet", 40)
  , ( "pades(\x0161)t"        , 50)
  , ( "(\x0289)edes(\x0161)t" , 60)
  , ( "sedmdes(\x0161)t"      , 70)
  , ( "osmndes(\x00161)t"     , 80)
  , ( "devades(\x0161)t"      , 90)
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(deset|dvacet|t(x\0281)icet|(x\0201)ty(x\0281)icet|pades(\x0161)t|(\x0289)edes(\x0161)t|sedmdes(\x0161)t|osmndes(\x00161)t|devades(\x0161)t"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) ruleNumeralMap >>= integer
      _ -> Nothing
  }



rules :: [Rule]
rules =
  [ ruleNumeral
  ]
