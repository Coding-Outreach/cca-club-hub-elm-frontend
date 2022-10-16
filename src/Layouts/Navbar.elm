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
    , body =
        E.column
            [ E.width E.fill
            , E.height E.fill
            , Bg.color mono_700
            , Font.color white
            , Font.size 16
            ]
            [ viewNavbar page.title, page.body ]
    }


viewNavbar : String -> Element msg
viewNavbar title =
    E.row
        [ Bg.color mono_800
        , E.spacing 32
        , E.paddingXY 32 16
        , E.width E.fill
        , Font.size 20
        ]
        [ E.link
            [ Font.bold
            , E.mouseOver [ Bg.color red_500 ]
            , Border.rounded 16
            , E.paddingXY 8 4
            ]
            { url = "/", label = E.text "CCA CLUB HUB" }
        , E.link [] { url = "/about", label = E.text "About" }
        , E.link [] { url = "/login", label = E.text "Login" }
        ]
