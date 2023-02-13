module Components.ProfilePicture exposing (..)

import Api
import Color exposing (..)
import Element as E exposing (Element)
import Element.Background as Bg
import Element.Border as Border


profilePicture : String -> String -> Element msg
profilePicture url clubName =
    E.image
        [ E.centerX
        , Border.rounded 160
        , E.clip
        , E.width (E.px 160)
        , E.height (E.px 160)
        , E.alignLeft
        , E.moveDown 64
        , E.moveRight 32
        , Bg.color mono_600
        ]
        { src = Api.backendUrl ++ url
        , description = clubName ++ "'s profile picture"
        }
