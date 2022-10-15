module Layouts.Navbar exposing (layout)

import Color exposing (..)
import Element as E exposing (Element)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import View exposing (View)


layout : { page : View msg } -> View msg
layout { page } =
    { title = page.title
    , body = E.column [ E.width E.fill, E.height E.fill ] [ viewNavbar page.title, page.body ]
    }


viewNavbar : String -> Element msg
viewNavbar title =
    E.row
        [ Bg.color mono_800
        , Font.color white
        , E.spacing 32
        , E.paddingXY 32 16
        , E.width E.fill
        ]
        [ E.link [ Font.bold, E.mouseOver [ Bg.color red_500 ], Border.rounded 16, E.paddingXY 8 4 ] { url = "/", label = E.text "CCA CLUB HUB" }
        , E.link [] { url = "/about", label = E.text "About" }
        , E.link [] { url = "/login", label = E.text "Login" }
        ]



-- Html.nav []
--     [ Html.ul []
--         [ Html.a [ Attr.href "/" ] [ text "Home" ]
--         , Html.a [ Attr.href "/about" ] [ text "About" ]
--         , Html.a [ Attr.href "/login" ] [ text "Login" ]
--         ]
--     ]
