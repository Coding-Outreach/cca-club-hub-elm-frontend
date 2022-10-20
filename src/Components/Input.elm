module Components.Input exposing (inputBoxStyles)

import Color exposing (..)
import Element as E
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font


inputBoxStyles : List (E.Attribute msg)
inputBoxStyles =
    [ Bg.color mono_600, Border.width 2, Border.color mono_500 ]
