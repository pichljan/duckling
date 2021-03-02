-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.GB.Corpus
  ( allExamples,
  )
where

import Data.String
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)
import Prelude

allExamples :: [Example]
allExamples =
  concat
    [ examples
        (datetime (2013, 2, 15, 0, 0, 0) Day)
        [ "15/2",
          "on 15/2",
          "15 / 2",
          "15-2",
          "15 - 2"
        ],
      examples
        (datetime (1974, 10, 31, 0, 0, 0) Day)
        [ "31/10/1974",
          "31/10/74",
          "31-10-74",
          "31.10.1974",
          "31 10 1974"
        ],
      examples
        (datetime (2013, 10, 10, 0, 0, 0) Day)
        [ "10/10",
          "10/10/2013"
        ],
      examples
        (datetime (2013, 4, 25, 16, 0, 0) Minute)
        [ "25/4 at 4:00pm"
        ]
    ]
