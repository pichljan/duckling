-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.Text as Text

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

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ("right now"    , TG.Second, 0  , "((právě|hned)\\s*)teď|nyní|teďka")
  , ("today"        , TG.Day   , 0  , "dnes|(v tento den)"           )
  , ("tomorrow"     , TG.Day   , 1  , "(zítra|zejtra)"            )
  , ("day after tomorrow"     , TG.Day   , 2  , "(pozítří|pozejtří)"            )
  , ("yesterday"    , TG.Day   , - 1, "včera"                      )
  , ("day before yesterday"     , TG.Day   , -2  , "(předevčírem|předvčírem)"            )
  ]

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "teď|v tento okamžik|teďka|nyní"
    ]
  , prod = \_ -> tt now
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Pondělí"   , "pondělí|po\\.?"         )
  , ( "Úterý"  , "úterý|út\\.?"      )
  , ( "Středa", "střed(a|u|y|ě|ou)|st\\.?"     )
  , ( "Čtvrtek" , "čtvrt(ek|ka|ku|kem)|čt\\.?" )
  , ( "Pátek"   , "pát(ek|ku|kem)|pá\\.?"         )
  , ( "Sobota" , "sobot(a|y|ě|u|ou)|so\\.?"       )
  , ( "Neděle"   , "neděl(e|i|í)|ne\\.?"         )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonthsWithLatent
  [ ( "Leden"  , "leden|led\\.?"    , False )
  , ( "Únor" , "únor|úno?\\.?"   , False )
  , ( "Březen"    , "březen|bře\\.?"      , False )
  , ( "Duben"    , "duben|dub\\.?"      , False )
  , ( "Květen"      , "květen|kvě\\.?"          , False  )
  , ( "Červen"     , "červen|čer\\.?"       , False )
  , ( "Červenec"     , "červenec|čerc\\.?"       , False )
  , ( "Srpen"   , "srpen|srp\\.?"     , False )
  , ( "Září", "září|zář?\\.?", False )
  , ( "Říjen"  , "říjen|říj\\.?"    , False )
  , ( "Listopad" , "listopad|lis\\.?"   , False )
  , ( "Prosinec" , "prosinec|pro\\.?"   , False )
  ]

ruleAbsorbOnDay :: Rule
ruleAbsorbOnDay = Rule
  { name = "on <day>"
  , pattern =
    [ regex "v"
    , Predicate $ isGrainOfTime TG.Day
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleAbsorbOnDay2 :: Rule
ruleAbsorbOnDay2 = Rule
  { name = "on <day>"
  , pattern =
    [ regex "ve"
    , Predicate $ isGrainOfTime TG.Day
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleNextDOW :: Rule
ruleNextDOW = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "(toto|tuto|tento|příští)"
    , Predicate isADayOfWeek
    ]
  , prod = \case
      (
        Token RegexMatch (GroupMatch (match:_)):
        Token Time dow:
        _) -> do
          td <- case Text.toLower match of
                  "tento" -> Just $ predNth 0 True dow
                  "příští" -> intersect dow $ cycleNth TG.Week 1
                  _ -> Nothing
          tt td
      _ -> Nothing
  }

ruleThisTime :: Rule
ruleThisTime = Rule
  { name = "this <time>"
  , pattern =
    [ regex "toto|tuto|tento|aktuální|probíhající"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 False td
      _ -> Nothing
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "příští"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "(uplynulý|uplynulé|uplynulou|minulý|minulé|minulou|předešlý|předešlé|předešlou)"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth (- 1) False td
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDDMM
  , ruleDDMMRRRR
  , ruleDDMMRRRRDot
  , ruleAbsorbOnDay
  , ruleAbsorbOnDay2
  , ruleNextDOW
  , ruleNextTime
  , ruleThisTime
  , ruleLastTime
  , ruleNow
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
