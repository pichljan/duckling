-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Time.CS.Rules
  ( rules,
  )
where

import Data.Maybe
import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Regex.Types
import Duckling.Time.Computed (easterSunday)
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types
import Prelude

ruleDDMM :: Rule
ruleDDMM =
  Rule
    { name = "dd/mm",
      pattern =
        [ regex "(3[01]|[12]\\d|0?[1-9])\\s?[/-]\\s?(1[0-2]|0?[1-9])"
        ],
      prod = \tokens -> case tokens of
        (Token RegexMatch (GroupMatch (dd : mm : _)) : _) -> do
          d <- parseInt dd
          m <- parseInt mm
          tt $ monthDay m d
        _ -> Nothing
    }

ruleDDMMRRRR :: Rule
ruleDDMMRRRR =
  Rule
    { name = "dd/mm/rrrr",
      pattern =
        [ regex "(3[01]|[12]\\d|0?[1-9])[-/\\s](1[0-2]|0?[1-9])[-/\\s](\\d{2,4})"
        ],
      prod = \tokens -> case tokens of
        (Token RegexMatch (GroupMatch (dd : mm : rr : _)) : _) -> do
          r <- parseInt rr
          d <- parseInt dd
          m <- parseInt mm
          tt $ yearMonthDay r m d
        _ -> Nothing
    }

ruleDDMMRRRRDot :: Rule
ruleDDMMRRRRDot =
  Rule
    { name = "dd.mm.rrrr",
      pattern =
        [ regex "(3[01]|[12]\\d|0?[1-9])\\.(1[0-2]|0?[1-9])\\.(\\d{4})"
        ],
      prod = \tokens -> case tokens of
        (Token RegexMatch (GroupMatch (dd : mm : rr : _)) : _) -> do
          r <- parseInt rr
          d <- parseInt dd
          m <- parseInt mm
          tt $ yearMonthDay r m d
        _ -> Nothing
    }

rules :: [Rule]
rules =
  [ ruleDDMM,
    ruleDDMMRRRR,
    ruleDDMMRRRRDot
  ]
