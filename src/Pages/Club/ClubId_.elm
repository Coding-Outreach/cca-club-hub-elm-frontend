module Pages.Club.ClubId_ exposing (page)

import Html exposing (Html)
import View exposing (View)


page : { clubId : String } -> View msg
page params =
    { title = "Pages.Club.ClubId_"
    , body = [ Html.text ("/club/" ++ params.clubId) ]
    }
