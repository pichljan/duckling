-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE GADTs #-}

module Duckling.Rules.CS
  ( defaultRules,
    langRules,
    localeRules,
  )
where

import Duckling.Dimensions.Types
import qualified Duckling.Distance.CS.Rules as Distance
import Duckling.Locale
import qualified Duckling.Numeral.CS.Rules as Numeral
import Duckling.Types
import qualified Duckling.Time.CS.Rules as Time

defaultRules :: Seal Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Seal Dimension -> [Rule]
localeRules region (Seal (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Seal Dimension -> [Rule]
langRules (Seal AmountOfMoney) = []
langRules (Seal CreditCardNumber) = []
langRules (Seal Distance) = Distance.rules
langRules (Seal Duration) = []
langRules (Seal Email) = []
langRules (Seal Numeral) = Numeral.rules
langRules (Seal Ordinal) = []
langRules (Seal PhoneNumber) = []
langRules (Seal Quantity) = []
langRules (Seal RegexMatch) = []
langRules (Seal Temperature) = []
langRules (Seal Time) = Time.rules
langRules (Seal TimeGrain) = []
langRules (Seal Url) = []
langRules (Seal Volume) = []
langRules (Seal (CustomDimension dim)) = dimLangRules CS dim
