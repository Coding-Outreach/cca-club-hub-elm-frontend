module Components.Icon exposing (icon)

import Element as E exposing (Element)
import Html
import Html.Attributes exposing (class)

icon : String -> Element msg
icon id =
    E.html (Html.i [ class id ] [])
