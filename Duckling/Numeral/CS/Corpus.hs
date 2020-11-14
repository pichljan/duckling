-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.CS.Corpus (corpus) where

import Data.String
import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types
import Prelude

corpus :: Corpus
corpus = (testContext {locale = makeLocale CS Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples =
  concat
    [ examples
        (NumeralValue 0)
        [ "0",
          "nula"
        ],
      examples
        (NumeralValue 1)
        [ "1",
          "jeden",
          "jedna",
          "jedno"
        ],
      examples
        (NumeralValue 2)
        [ "dva",
          "dvĕ"
        ],
      examples
        (NumeralValue 3)
        [ "tři"
        ],
      examples
        (NumeralValue 4)
        [ "čtyři"
        ],
      examples
        (NumeralValue 7)
        [ "7",
          "sedm"
        ],
      examples
        (NumeralValue 14)
        [ "14",
          "čtrnáct"
        ],
      examples
        (NumeralValue 16)
        [ "16",
          "šestnácť"
        ],
      examples
        (NumeralValue 17)
        [ "17",
          "sedmnáct"
        ],
      examples
        (NumeralValue 18)
        [ "18",
          "osmnáct"
        ],
      examples
        (NumeralValue 20)
        [ "20",
          "dvacet"
        ],
      examples
        (NumeralValue 1.1)
        [ "1,1",
          "1,10",
          "01,10"
        ],
      examples
        (NumeralValue 0.77)
        [ "0,77",
          ",77"
        ],
      examples
        (NumeralValue 100000)
        [ "100.000",
          "100000",
          "100K",
          "100k"
        ],
      examples
        (NumeralValue 3000000)
        [ "3M",
          "3000K",
          "3000000",
          "3.000.000"
        ],
      examples
        (NumeralValue 1200000)
        [ "1.200.000",
          "1200000",
          "1,2M",
          "1200K",
          ",0012G"
        ],
      examples
        (NumeralValue (-1200000))
        [ "- 1.200.000",
          "-1200000",
          "mínus 1.200.000",
          "-1,2M",
          "-1200K",
          "-,0012G"
        ],
      examples
        (NumeralValue 5000)
        [ "5 tisíc",
          "pět tisíc"
        ]
    ]
