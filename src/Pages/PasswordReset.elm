module Pages.PasswordReset exposing (Model, Msg, page)

import Api
import Array exposing (Array)
import Color exposing (..)
import Components.Icon exposing (icon)
import Components.Input as CInput
import Effect exposing (Effect)
import Element as E exposing (Element, el, text)
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import Http
import Layout exposing (Layout)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Set exposing (Set)
import Shared
import Shared.Model exposing (LoginStatus(..))
import Token exposing (getClubIdFromToken)
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


type Msg
    = UpdatePassword String
    | UpdateConfirmation String
    | Submit
    | ResetResponse (Result Http.Error ())
    | TogglePassword
    | ToggleConfirmation


type alias Model =
    { password : String
    , confirmation : String
    , showPassword : Bool
    , showConfirmation : Bool
    , token : String
    , response : Maybe (Result Http.Error ())
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    let
        initial =
            { password = ""
            , confirmation = ""
            , showPassword = False
            , showConfirmation = False
            , token = ""
            , response = Nothing
            }
    in
    case shared.loginStatus of
        Shared.Model.NotLoggedIn ->
            ( initial
            , Effect.pushUrlPath "/"
            )

        Shared.Model.LoggedIn { clubId, token } ->
            ( { initial | token = token }
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UpdatePassword password ->
            ( { model | password = password }, Effect.none )

        UpdateConfirmation password ->
            ( { model | confirmation = password }, Effect.none )

        TogglePassword ->
            ( { model | showPassword = not model.showPassword }, Effect.none )

        ToggleConfirmation ->
            ( { model | showConfirmation = not model.showConfirmation }, Effect.none )

        Submit ->
            if (String.length model.password + String.length model.confirmation /= 0) && (model.password == model.confirmation) then
                ( model
                , Api.sendPasswordReset model.token model.password ResetResponse
                    |> Effect.sendCmd
                )

            else
                ( model, Effect.none )

        ResetResponse res ->
            ( { model | response = Just res }, Effect.none )


fieldIcon : Bool -> String
fieldIcon show =
    if show then
        "fa-eye"

    else
        "fa-eye-slash"


view : Model -> View Msg
view model =
    { title = "Password Reset"
    , body =
        case model.response of
            Just (Ok _) ->
                el [ E.centerX, E.centerY ] (text "Password reset successfully")

            Just (Err status) ->
                el [ E.centerX, E.centerY ]
                    (Debug.toString status
                        |> text
                    )

            _ ->
                E.column [ E.centerX, E.centerY ]
                    [ el [] (E.text "Password Reset")
                    , CInput.currentPassword
                        [ E.inFront
                            (el
                                [ E.centerY
                                , E.alignRight
                                , E.paddingXY 8 0
                                , Font.color mono_200
                                , E.mouseOver [ Font.color mono_400 ]
                                , E.pointer
                                , Events.onClick TogglePassword
                                ]
                                (icon ("fa-regular " ++ fieldIcon model.showPassword))
                            )
                        ]
                        { onChange = UpdatePassword
                        , text = model.password
                        , placeholder = Nothing
                        , label = "PASSWORD"
                        , show = model.showPassword
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
                                , Events.onClick ToggleConfirmation
                                ]
                                (icon ("fa-regular " ++ fieldIcon model.showConfirmation))
                            )
                        ]
                        { onChange = UpdateConfirmation
                        , text = model.confirmation
                        , placeholder = Nothing
                        , label = "PASSWORD CONFIRMATION"
                        , show = model.showConfirmation
                        }
                    , CInput.button [] { onPress = Just Submit, label = text "Reset" }
                    ]
    }
