module Icon exposing (..)

import Element as E exposing (Element)
import Html
import Html.Attributes exposing (class)

-- Maybe move this into a components folder
icon : String -> Element msg
icon id =
    E.html (Html.i [ class id ] [])
