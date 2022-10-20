module Components.Input exposing (inputBoxStyles)

import Element as E
import Element.Font as Font
import Element.Background as Bg
import Element.Border as Border
import Color exposing (..)

inputBoxStyles : List (E.Attribute msg)
inputBoxStyles = [ Bg.color mono_600, Border.width 2, Border.color mono_500 ]