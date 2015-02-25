module Unison.Var where

import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder)

type alias I = Int

bound1 : I
bound1 = 1

succ : I -> I
succ i = i + 1

decr : I -> I
decr i = (i - 1) `max` 0

decode : Decoder I
decode = Decoder.int

encode : Encoder I
encode i = Encoder.float (toFloat i)
