module Pages.NotFound_ exposing (page)

import Layout exposing (Layout)
import View exposing (View)


layout : Layout
layout =
    Layout.Navbar


page : View msg
page =
    View.fromString "Page not found."
