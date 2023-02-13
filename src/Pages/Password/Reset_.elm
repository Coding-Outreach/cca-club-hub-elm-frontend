module Pages.Password.Reset_ exposing (Model, Msg, page)

import Api
import Color exposing (..)
import Components.Icon exposing (icon)
import Components.Input as CInput
import Effect exposing (Effect)
import Element as E exposing (el, text)
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import Http
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model exposing (LoginStatus(..))
import View exposing (View)


page : Shared.Model -> Route { reset : String } -> Page Model Msg
page _ route =
    Page.new
        { init = init route
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\_ -> Layouts.Navbar { navbar = {} })


type Msg
    = UpdatePassword String
    | UpdateConfirmation String
    | Submit
    | ResetResponse (Result Api.Error ())
    | Validate (Result Api.Error ())
    | TogglePassword
    | ToggleConfirmation


type alias Model =
    { password : String
    , confirmation : String
    , showPassword : Bool
    , showConfirmation : Bool
    , uid : String
    , response : Maybe (Result Api.Error ())
    , validate : Maybe (Result Api.Error ())
    }


init : Route { reset : String } -> () -> ( Model, Effect Msg )
init route _ =
    let
        initial =
            { password = ""
            , confirmation = ""
            , showPassword = False
            , showConfirmation = False
            , uid = route.params.reset
            , response = Nothing
            , validate = Nothing
            }
    in
    ( initial
    , Api.checkResetUrl route.params.reset Validate
        |> Effect.sendCmd
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
                , Api.sendPasswordReset model.uid model.password ResetResponse
                    |> Effect.sendCmd
                )

            else
                ( model, Effect.none )

        ResetResponse res ->
            ( { model | response = Just res }, Effect.none )

        Validate res ->
            ( { model | validate = Just res }, Effect.none )


fieldIcon : Bool -> String
fieldIcon show =
    if show then
        "fa-eye-slash"

    else
        "fa-eye"


view : Model -> View Msg
view model =
    { title = "Password Reset"
    , body =
        case model.validate of
            Just (Ok _) ->
                case model.response of
                    Just (Ok _) ->
                        el [ E.centerX, E.centerY ] (text "Password reset successfully")

                    Just (Err err) ->
                        el [ E.centerX, E.centerY ]
                            (Api.errorToString err
                                |> text
                            )

                    _ ->
                        E.column [ E.centerX, E.centerY, E.spacing 8, E.width (E.fill |> E.maximum (16 * 24)) ]
                            [ el
                                [ E.paddingXY 0 8
                                , Font.size 28
                                , Font.bold
                                , Region.heading 1
                                ]
                                (E.text "Password Reset")
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

            _ ->
                el [ E.centerX, E.centerY ] (text "Invalid password reset url")
    }
