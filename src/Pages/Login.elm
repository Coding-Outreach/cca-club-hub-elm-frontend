module Pages.Login exposing (Model, Msg, page)

import Api
import Color exposing (..)
import Dict
import Effect exposing (Effect)
import Element as E exposing (el, text)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Http
import Icon exposing (icon)
import Json.Encode as E
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Msg
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
    { username : String
    , password : String
    , showPassword : Bool
    , badLogin : String
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( Model "" "" False ""
    , if shared.loginStatus /= Shared.NotLoggedIn then
        Effect.pushUrlPath "/"
      else
        Effect.none
    )



-- UPDATE


type Msg
    = FieldUpdate Field String
    | ToggleShow
    | Submit
    | GotResponse (Result Http.Error Api.LoginResponse)


type Field
    = Username
    | Password


fieldToString : Field -> String
fieldToString field =
    case field of
        Username ->
            "Username"

        Password ->
            "Password"


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        FieldUpdate Username username ->
            ( { model | username = username }, Effect.none )

        FieldUpdate Password password ->
            ( { model | password = password }, Effect.none )

        ToggleShow ->
            ( { model | showPassword = not model.showPassword }, Effect.none )

        Submit ->
            ( model, Effect.fromCmd (Api.doLogin model.username model.password GotResponse) )

        GotResponse (Ok res) ->
            ( model
            , Effect.batch
                [ Effect.save { key = "token", value = E.string res.token }
                , Effect.fromSharedMsg (Shared.Msg.Login res)
                , Effect.pushRoute
                    { path = Route.Path.Club__ClubId_ { clubId = res.clubId }
                    , query = Dict.empty
                    , hash = Nothing
                    }
                ]
            )

        GotResponse (Err err) ->
            case err of
                Http.BadUrl url ->
                    ( { model | badLogin = "Bad Url: " ++ url }, Effect.none )

                Http.BadStatus 401 ->
                    ( { model | badLogin = "Invalid username or password" }, Effect.none )

                Http.BadStatus 404 ->
                    ( { model | badLogin = "Backend not found" }, Effect.none )

                Http.BadStatus code ->
                    ( { model | badLogin = "Error. HTTP status code: " ++ String.fromInt code }, Effect.none )

                Http.BadBody error ->
                    ( { model | badLogin = "Failed to parse response: " ++ error }, Effect.none )

                Http.Timeout ->
                    ( { model | badLogin = "Timed Out" }, Effect.none )

                Http.NetworkError ->
                    ( { model | badLogin = "Network error" }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    let
        eyeIcon =
            if model.showPassword then
                "fa-eye-slash"

            else
                "fa-eye"
    in
    { title = "Login"
    , body =
        E.column [ E.centerX, E.centerY, E.spacing 8, E.width (E.fill |> E.maximum (16 * 24)) ]
            [ el
                [ E.paddingXY 0 8
                , Font.size 28
                , Font.bold
                , Region.heading 1
                ]
                (E.text "Welcome Back!")
            , Input.username [ Font.color mono_900 ]
                { onChange = FieldUpdate Username
                , text = model.username
                , placeholder = Nothing
                , label = Input.labelAbove [ Font.bold ] (text "USERNAME")
                }
            , Input.currentPassword
                [ Font.color mono_900
                , E.inFront
                    (el
                        [ E.centerY
                        , E.alignRight
                        , E.paddingXY 8 0
                        , Font.color mono_200
                        , E.mouseOver [Font.color mono_400]
                        , E.pointer
                        , Events.onClick ToggleShow
                        ]
                        (icon ("fa-regular " ++ eyeIcon))
                    )
                ]
                { onChange = FieldUpdate Password
                , text = model.password
                , placeholder = Nothing
                , label = Input.labelAbove [ Font.bold ] (text "PASSWORD")
                , show = model.showPassword
                }
            , E.link [ Font.underline, Font.color red_400 ] { url = "/forgot-password", label = text "Forgot your password?" }
            , Input.button
                [ Bg.color red_500
                , E.paddingXY 16 12
                , Font.size 18
                , Font.bold
                , Border.rounded 4
                ]
                { onPress = Just Submit, label = text "Login" }
            , el [ Font.color red_300 ] (text model.badLogin)
            ]
    }
