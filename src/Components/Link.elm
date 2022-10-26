module Components.Link exposing (linkStyles)

import Color exposing (..)
import Element as E
import Element.Font as Font

linkStyles : List (E.Attribute msg)
linkStyles = [Font.underline, Font.color red_400]