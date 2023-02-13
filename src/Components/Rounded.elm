module Components.Rounded exposing (..)

import Element as E
import Element.Border as Border
import Html.Attributes


rounded : List (E.Attribute msg)
rounded =
    [ Border.rounded 8, E.clip, E.htmlAttribute (Html.Attributes.style "flex-basis" "auto") ]
