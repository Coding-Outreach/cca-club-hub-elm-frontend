module Pages.Club.ClubId_ exposing (Model, Msg, page)

import Api
import Color exposing (..)
import Components.Icon exposing (icon)
import Effect exposing (Effect)
import Element as E exposing (Element, el, text)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Http
import Layout exposing (Layout)
import Markdown
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


layout : Layout
layout =
    Layout.Navbar


page : Shared.Model -> Route { clubId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init route
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT
-- do we need clubId? its already in clubInfo, i guess for the title, but not really
-- TODO refactor Model into a Api.Status Api.ClubInfoResponse


type alias Model =
    { clubId : String
    , clubInfo : Api.Status Api.ClubInfo
    }


init : Route { clubId : String } -> () -> ( Model, Effect Msg )
init route () =
    ( { clubId = route.params.clubId
      , clubInfo = Api.Loading
      }
    , Effect.fromCmd (Api.getClubInfo route.params.clubId GotResponse)
    )



-- UPDATE


type Msg
    = GotResponse (Result Http.Error Api.ClubInfo)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotResponse (Ok res) ->
            ( { model | clubInfo = Api.Success res }
            , Effect.none
            )

        GotResponse (Err err) ->
            ( { model | clubInfo = Api.Failure err }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    let
        title =
            case model.clubInfo of
                Api.Success info ->
                    info.clubName

                _ ->
                    model.clubId
    in
    { title = title
    , body =
        case model.clubInfo of
            Api.Loading ->
                el [ E.centerX, E.centerY ] (text "Loading...")

            Api.Failure err ->
                case err of
                    Http.BadStatus 404 ->
                        E.column [ E.centerX, E.centerY ]
                            [ text "Club not found!"
                            , E.link [ Font.color red_300, Font.underline, E.centerX ] { url = "/", label = text "Go Back" }
                            ]

                    _ ->
                        el [ E.centerX, E.centerY ] (text ("Something went wrong: " ++ Debug.toString err))

            Api.Success info ->
                E.column [ E.padding 32, E.width E.fill, E.height E.fill ]
                    [ el [ E.width E.fill, E.height (E.px 128), Bg.color red_100, E.padding 16 ] (text "")
                    , E.column
                        [ E.width E.fill
                        , Bg.color mono_600
                        , E.paddingEach
                            { top = 72
                            , right = 32
                            , bottom = 32
                            , left = 32
                            }
                        , E.above (viewProfilePicture info.profilePictureUrl info.clubName)
                        ]
                        [ E.column [ E.spacing 12 ]
                            [ el [ Font.bold, Font.size 32, Region.heading 1 ] (text info.clubName)
                            , E.row [ Font.color mono_100 ] [ icon "fa-regular fa-clock", el [] (text (" " ++ info.meetTime)) ]
                            ]
                        , E.paragraph
                            [ E.paddingEach
                                { top = 16
                                , right = 0
                                , bottom = 0
                                , left = 0
                                }
                            ]
                            [ text info.description ]
                        ]
                    , E.row [ E.padding 32, E.width E.fill ]
                        [ E.column
                            [ E.spacing 8
                            , E.width (E.fillPortion 4)
                            ]
                            (el [ Font.color mono_300, Font.size 12, Font.bold ] (text "ABOUT")
                                :: List.map E.html (Markdown.toHtml Nothing info.about)
                             -- We can't just throw this into the E.html since links need to look nice, another option is to add a css file specificially for markdown
                            )
                        , E.column
                            [ E.spacing 8
                            , E.alignRight
                            , E.alignTop
                            , E.width (E.fillPortion 1)
                            ]
                            [ el [ Font.color mono_300, Font.size 12, Font.bold ] (text "TAGS")
                            , E.wrappedRow
                                [ E.spacing 8
                                ]
                                (List.map viewCategory info.categories)
                            ]
                        ]
                    ]
    }



-- TODO add a border with a container


viewProfilePicture : String -> String -> Element msg
viewProfilePicture url clubName =
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
        { src = url
        , description = clubName ++ "'s profile picture"
        }


viewCategory : String -> Element Msg
viewCategory category =
    E.link
        [ E.paddingXY 12 6
        , Border.rounded 16
        , Bg.color red_100
        , Font.color red_700
        , Font.bold
        , Font.size 12
        , E.mouseOver [ Bg.color red_200 ]
        ]
        { url = "/tag/" ++ category, label = text (String.toUpper category) }
