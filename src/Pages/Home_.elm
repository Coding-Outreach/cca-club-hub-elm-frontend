module Pages.Home_ exposing (Model, Msg, page)

import Api
import Array exposing (Array)
import Color exposing (..)
import Components.Icon exposing (icon)
import Effect exposing (Effect)
import Element as E exposing (el, text)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html.Attributes as Attr
import Http
import Layout exposing (Layout)
import Layouts
import Markdown
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\_ -> Layouts.Navbar { navbar = {} })



-- INIT


type alias Model =
    { clubs : Api.Status Api.FeaturedClubsResponse
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { clubs = Api.Loading }
    , Effect.sendCmd (Api.getFeaturedClubList GotClubs)
    )



-- UPDATE


type Msg
    = GotClubs (Result Http.Error Api.FeaturedClubsResponse)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotClubs (Ok clubs) ->
            ( { model | clubs = Api.Success clubs }, Effect.none )

        GotClubs (Err err) ->
            ( { model | clubs = Api.Failure err }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Home"
    , body =
        case model.clubs of
            Api.Loading ->
                el [ E.centerX, E.centerY ] (text "Loading...")

            Api.Failure err ->
                el [ E.centerX, E.centerY ] (text ("Oh no! Something went wrong: " ++ Debug.toString err))

            Api.Success list ->
                E.column [ E.padding 32, E.width E.fill, E.height E.fill ]
                    [ E.column [ E.width (E.fill |> E.maximum (16 * 56)), E.height (E.fill |> E.maximum (16 * 28)), E.centerX ]
                        [ list
                            |> Array.get 0
                            |> Maybe.map viewClub
                            |> Maybe.withDefault (text "")
                        ]
                    ]
    }


viewClub : Api.ClubInfo -> E.Element msg
viewClub info =
    E.column [ E.width E.fill, E.height E.fill ]
        [ el [ E.width E.fill, E.height (E.px 128), Bg.color red_100, E.padding 16 ] (text "")
        , E.column
            [ E.width E.fill
            , E.height E.fill
            , Bg.color mono_600
            , E.paddingEach
                { top = 80
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
            , E.column [ E.spacing 16 ]
                [ E.paragraph
                    [ E.paddingEach
                        { top = 16
                        , right = 0
                        , bottom = 0
                        , left = 0
                        }
                    , E.spacing 8
                    ]
                    [ text info.description ]
                , el [ Font.color mono_300, Font.size 12, Font.bold ] (text "ABOUT")
                , info.about
                    |> Markdown.toHtml Nothing
                    |> List.map E.html
                    |> E.paragraph
                        [ E.htmlAttribute (Attr.style "text-overflow" "ellipsis")
                        , E.htmlAttribute (Attr.style "overflow" "hidden")
                        , E.height (E.fill |> E.maximum (16 * 3))
                        ]
                ]
            ]
        ]


viewProfilePicture : String -> String -> E.Element msg
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
