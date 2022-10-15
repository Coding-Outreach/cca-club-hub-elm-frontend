module Pages.Login exposing (Model, Msg, page)

import Api
import Effect exposing (Effect)
import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Encode as E
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Msg
import View exposing (View)
import Route.Path
import Dict


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
    { username : String
    , password : String
    , badLogin : String
    }


init : () -> ( Model, Effect Msg )
init () =
    ( Model "" "" ""
    , Effect.none
    )



-- UPDATE


type Msg
    = FieldUpdate Field String
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
            ( { model | username = username }
            , Effect.none
            )

        FieldUpdate Password password ->
            ( { model | password = password }
            , Effect.none
            )

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
    { title = "Pages.Login"
    , body =
        [ Html.div []
            [ Html.div []
                [ Html.h3 [] [ text "Welcome Back!" ]
                , Html.form [ Events.onSubmit Submit ]
                    [ viewInput Username "text" model.username
                    , viewInput Password "password" model.password
                    , Html.a [ Attr.href "reset-password" ] [ text "Forgot your password?" ]
                    , Html.button [] [ text "Login" ]
                    ]
                , Html.div [] [ text model.badLogin ]
                ]
            ]
        ]
    }


viewInput : Field -> String -> String -> Html Msg
viewInput field t v =
    let
        id =
            "login-" ++ String.toLower (fieldToString field)
    in
    Html.div []
        [ Html.label [ Attr.for id ] [ text (fieldToString field) ]
        , Html.input
            [ Attr.id id
            , Attr.name id
            , Attr.type_ t
            , Attr.value v
            , Events.onInput (FieldUpdate field)
            ]
            []
        ]
