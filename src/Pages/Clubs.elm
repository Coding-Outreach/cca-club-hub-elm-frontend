module Pages.Clubs exposing (Model, Msg, page)

import Api
import Color exposing (..)
import Effect exposing (Effect)
import Element as E exposing (el, text)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Http
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


layout : Layout
layout =
    Layout.Navbar


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { searchTerm : String
    , clubList : Api.Status Api.ClubListResponse
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { searchTerm = ""
      , clubList = Api.Loading
      }
    , Effect.fromCmd (Api.getClubList GotResponse)
    )



-- UPDATE


type Msg
    = SearchTermChange String
    | GotResponse (Result Http.Error Api.ClubListResponse)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SearchTermChange searchTerm ->
            ( { model | searchTerm = searchTerm }
            , Effect.none
            )

        GotResponse (Err err) ->
            ( { model | clubList = Api.Failure err }, Effect.none )

        GotResponse (Ok list) ->
            ( { model | clubList = Api.Success list }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "search"
    , body =
        case model.clubList of
            Api.Loading ->
                el [ E.centerX, E.centerY ] (text "Loading...")

            Api.Failure err ->
                el [ E.centerX, E.centerY ] (text ("Oh no! Something went wrong: " ++ Debug.toString err))

            Api.Success list ->
                E.column [ E.padding 32, E.width E.fill, E.height E.fill ]
                    [ Input.search
                        [ E.width (E.fill |> E.maximum (16 * 30))
                        , E.centerX
                        , Font.color mono_900
                        ]
                        { onChange = SearchTermChange
                        , text = model.searchTerm
                        , placeholder = Nothing
                        , label =
                            Input.labelAbove
                                [ E.paddingXY 0 8
                                , Font.size 28
                                , Font.bold
                                , Region.heading 1
                                ]
                                (E.text "Search for clubs:")
                        }
                    ]
    }
