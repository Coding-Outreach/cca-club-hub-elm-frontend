module Pages.NotFound_ exposing (page)

import View exposing (View)
import Layout exposing (Layout)

layout : Layout
layout = Layout.Navbar

page : View msg
page =
    View.fromString "Page not found."