module Layouts.Navbar exposing (layout)

import Html exposing (Html, text)
import Html.Attributes as Attr
import View exposing (View)


layout : { page : View msg } -> View msg
layout { page } =
    { title = page.title
    , body = [ viewNavbar page.title, Html.div [ Attr.class "page" ] page.body ]
    }


viewNavbar : String -> Html msg
viewNavbar title =
    Html.nav []
        [ Html.ul []
            [ Html.a [ Attr.href "/" ] [ text "Home" ]
            , Html.a [ Attr.href "/about" ] [ text "About" ]
            , Html.a [ Attr.href "/login" ] [ text "Login" ]
            ]
        ]
