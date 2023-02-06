module Pages.PasswordReset exposing (Model, Msg, page)

import Api
import Array exposing (Array)
import Color exposing (..)
import Components.Input as CInput
import Effect exposing (Effect)
import Element as E exposing (Element, el, text)
import Http
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


type Msg
    = UpdatePassword Int String
    | Submit
    | ResetResponse (Result Http.Error ())


type alias Model =
    { passwords : Array String
    , token : String
    , response : Maybe (Result Http.Error ())
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    let
        initial =
            { passwords = Array.repeat 2 ""
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
        UpdatePassword index password ->
            ( { model | passwords = Array.set index password model.passwords }, Effect.none )

        Submit ->
            let
                passwords =
                    Array.toList model.passwords
                        |> Set.fromList
            in
            case Set.toList passwords of
                password :: [] ->
                    ( model
                    , Api.sendPasswordReset model.token password ResetResponse
                        |> Effect.sendCmd
                    )

                _ ->
                    ( model, Effect.none )

        ResetResponse res ->
            ( { model | response = Just res }, Effect.none )


passwordField : Model -> Int -> Element Msg
passwordField model index =
    let
        label =
            if index == 0 then
                "password"

            else
                "password confirmation"
    in
    CInput.currentPassword []
        { onChange = UpdatePassword index
        , text =
            Array.get index model.passwords
                |> Maybe.withDefault ""
        , placeholder = Nothing
        , label = label
        , show = True
        }


view : Model -> View Msg
view model =
    { title = "Password Reset"
    , body =
        case model.response of
            Just Ok _ ->
                el [ E.centerX, E.centerY ] (text "Password reset successfully")

            Just Err status ->
                el [ E.centerX, E.centerY ]
                    (Debug.toString status
                        |> text
                    )

            _ ->
                let
                    fields =
                        Array.length model.passwords
                            - 1
                            |> List.range 0
                            |> List.map (passwordField model)
                in
                E.column [ E.centerX, E.centerY ]
                    ([ el [] (E.text "Password Reset")
                     ]
                        ++ fields
                        ++ [ CInput.button [] { onPress = Just Submit, label = text "Reset" }
                           ]
                    )
    }
