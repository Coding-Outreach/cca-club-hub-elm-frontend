module Pages.Home_ exposing (Model, Msg, page)

import Api
import Array exposing (Array)
import Color exposing (..)
import Components.Icon exposing (icon)
import Effect exposing (Effect)
import Element as E exposing (el, text)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Events
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
import Time
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
    , index : Int
    , countdown : Int
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { clubs = Api.Loading, index = 0, countdown = 5 }
    , Effect.sendCmd (Api.getFeaturedClubList GotClubs)
    )



-- UPDATE


type Msg
    = GotClubs (Result Http.Error Api.FeaturedClubsResponse)
    | Select Int
    | CountDown Time.Posix


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotClubs (Ok clubs) ->
            ( { model | clubs = Api.Success clubs }, Effect.none )

        GotClubs (Err err) ->
            ( { model | clubs = Api.Failure err }, Effect.none )

        Select i ->
            ( { model | index = i, countdown = 10 }, Effect.none )

        CountDown _ ->
            let
                newCountdown =
                    if model.countdown <= 0 then
                        5

                    else
                        model.countdown - 1

                newIndex =
                    if model.countdown <= 0 then
                        modBy (Api.map Array.length model.clubs |> Api.withDefault model.index) (model.index + 1)

                    else
                        model.index
            in
            ( { model | index = newIndex, countdown = newCountdown }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 CountDown



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

            Api.Success clubs ->
                E.column [ E.padding 32, E.width E.fill, E.height E.fill ]
                    [ E.column [ E.width (E.fill |> E.maximum (16 * 56)), E.height (E.fill |> E.maximum (16 * 28)), E.centerX ]
                        [ el
                            [ E.paddingXY 0 16
                            , Font.size 28
                            , Font.bold
                            , Region.heading 1
                            ]
                            (E.text "Featured Clubs")
                        , clubs
                            |> Array.get model.index
                            |> Maybe.map viewClub
                            |> Maybe.withDefault (text "")
                        , E.el [ E.height (E.px 20), Bg.color mono_600, E.width E.fill ]
                            (E.row [ E.spacing 8, E.centerX ]
                                (Array.indexedMap
                                    (\i _ ->
                                        E.el
                                            [ E.height (E.px 12)
                                            , E.width (E.px 12)
                                            , Border.rounded 128
                                            , if i == model.index then
                                                Bg.color mono_400

                                              else
                                                Border.color mono_200
                                            , if i /= model.index then
                                                Border.width 2

                                              else
                                                Border.width 0
                                            , Events.onClick (Select i)
                                            ]
                                            E.none
                                    )
                                    clubs
                                    |> Array.toList
                                )
                            )
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
