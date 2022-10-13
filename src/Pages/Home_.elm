module Pages.Home_ exposing (page)

import Html
import Layout exposing (Layout)
import View exposing (View)


layout : Layout
layout =
    Layout.Navbar


page : View msg
page =
    { title = "Homepage"
    , body = [ Html.text "Hello, world!" ]
    }
