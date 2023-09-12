module Components.ClubListing exposing (listing, listings)

import Api
import Color exposing (..)
import Components.Icon exposing (..)
import Components.Rounded
import Element as E exposing (Element, el, text)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Keyed as Keyed
import Element.Region as Region


listing : Api.ClubListItem -> ( String, Element msg )
listing item =
    ( item.id
    , E.link (Components.Rounded.rounded ++ [ E.width E.fill, Bg.color mono_600 ])
        { url = "/club/" ++ item.id
        , label =
            E.row [ E.padding 16, E.spacing 16 ]
                [ E.image
                    [ E.centerX
                    , Border.rounded 96
                    , E.clip
                    , E.width (E.px 96)
                    , E.height (E.px 96)
                    , E.alignLeft
                    ]
                    { src = Api.backendUrl ++ item.profilePictureUrl
                    , description = item.clubName ++ "'s profile picture"
                    }
                , E.column [ E.spacing 12, E.width E.fill ]
                    [ el [ Font.bold, Font.size 32, Region.heading 1 ] (text item.clubName)
                    , E.row [ Font.color mono_100 ] [ icon "fa-regular fa-clock", el [] (text (" " ++ item.meetTime)) ]
                    , E.paragraph
                        [ E.paddingEach
                            { top = 8
                            , right = 0
                            , bottom = 0
                            , left = 0
                            }
                        ]
                        [ text item.description ]
                    ]
                ]
        }
    )


listings : List Api.ClubListItem -> Element msg
listings items =
    Keyed.column
        [ E.width (E.fill |> E.maximum (16 * 48))
        , E.centerX
        , E.spacing 16
        ]
        (List.map listing items)
