module Pages.Login exposing (Model, Msg, page)

import Api
import Color exposing (..)
import Components.Icon exposing (icon)
import Components.Input as CInput
import Components.Link exposing (linkStyles)
import Effect exposing (Effect)
import Element as E exposing (el, text)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Http
import Layout exposing (Layout)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\_ -> Layouts.Navbar { navbar = {} })



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
    , if shared.loginStatus /= Shared.Model.NotLoggedIn then
        Effect.pushUrlPath "/"

      else
        Effect.none
    )



-- UPDATE


type Msg
    = FieldUpdate Field String
    | ToggleShow
    | Submit
    | GotResponse (Result Api.Error Api.LoginResponse)


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
            ( model, Effect.sendCmd (Api.doLogin model.username model.password GotResponse) )

        GotResponse (Ok res) ->
            ( model
            , Effect.sendSharedMsg (Shared.Msg.Login res)
            )

        GotResponse (Err err) ->
            ( { model | badLogin = Api.errorToString err }, Effect.none )



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
            , CInput.username []
                { onChange = FieldUpdate Username
                , text = model.username
                , placeholder = Nothing
                , label = "USERNAME"
                }
            , CInput.currentPassword
                [ E.inFront
                    (el
                        [ E.centerY
                        , E.alignRight
                        , E.paddingXY 8 0
                        , Font.color mono_200
                        , E.mouseOver [ Font.color mono_400 ]
                        , E.pointer
                        , Events.onClick ToggleShow
                        ]
                        (icon ("fa-regular " ++ eyeIcon))
                    )
                ]
                { onChange = FieldUpdate Password
                , text = model.password
                , placeholder = Nothing
                , label = "PASSWORD"
                , show = model.showPassword
                }
            , E.link linkStyles { url = "/password/request", label = text "Forgot your password?" }
            , CInput.button [] { onPress = Just Submit, label = text "Login" }
            , el [ Font.color red_300 ] (text model.badLogin)
            ]
    }
