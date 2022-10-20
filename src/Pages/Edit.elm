module Pages.Edit exposing (Model, Msg, page)

import Api
import Color exposing (..)
import Components.Input exposing (inputBoxStyles)
import Effect exposing (Effect)
import Element as E exposing (el, text)
import Element.Background as Input
import Element.Font as Font
import Element.Input as Input
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
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    Api.Status { clubName : String, description : String }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( Api.Loading
    , case shared.loginStatus of
        Shared.NotLoggedIn ->
            Effect.pushUrlPath "/"

        Shared.LoggedIn { clubId } ->
            Effect.fromCmd (Api.getClubInfo clubId GotInitialData)
    )



-- UPDATE


type Field
    = ClubName String
    | Description String


type Msg
    = GotInitialData (Result Http.Error Api.ClubInfoResponse)
    | FieldUpdate Field


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotInitialData (Ok info) ->
            ( Api.Success
                { clubName = info.clubName
                , description = Maybe.withDefault "" info.description
                }
            , Effect.none
            )

        GotInitialData (Err err) ->
            ( Api.Failure err
            , Effect.none
            )

        FieldUpdate (ClubName name) ->
            ( Api.map (\m -> { m | clubName = name }) model
            , Effect.none
            )

        FieldUpdate (Description description) ->
            ( Api.map (\m -> { m | description = String.left 280 description }) model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Login"
    , body =
        case model of
            Api.Loading ->
                el [ E.centerX, E.centerY ] (text "Loading...")

            Api.Failure err ->
                el [ E.centerX, E.centerY ] (text ("Something went wrong: " ++ Debug.toString err))

            Api.Success info ->
                E.column [ E.padding 32, E.width (E.fill |> E.maximum (16 * 36)), E.height E.fill, E.centerX, E.spacing 16 ]
                    [ Input.text inputBoxStyles
                        { onChange = ClubName >> FieldUpdate
                        , text = info.clubName
                        , placeholder = Nothing
                        , label = Input.labelAbove [ Font.bold, Font.color mono_400, Font.size 14 ] (text "DISPLAY NAME")
                        }

                    -- add character limits
                    , Input.multiline
                        (E.inFront
                            (el
                                [ E.alignBottom
                                , E.alignRight
                                , E.padding 8
                                , Font.color mono_400
                                ]
                                (text (String.fromInt (280 - String.length info.description)))
                            )
                            :: inputBoxStyles
                        )
                        { onChange = Description >> FieldUpdate
                        , text = info.description
                        , placeholder = Nothing
                        , label = Input.labelAbove [ Font.bold, Font.color mono_400, Font.size 14 ] (text "DESCRIPTION")
                        , spellcheck = True
                        }
                    ]
    }
