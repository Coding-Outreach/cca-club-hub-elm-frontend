module Pages.ForgotPassword exposing (Model, Msg, page)

import Api
import Effect exposing (Effect)
import Http
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)
import Element as E exposing (el, text)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Components.Input as CInput


page : Shared.Model -> Route () -> Page Model Msg
page _ route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { email : Maybe String
    , requested : Bool
    , response : Result Http.Error ()
    }


type Msg
    = UpdateEmail String
    | Submit
    | Response (Result Http.Error ())


init : () -> ( Model, Effect Msg )
init _ =
    ( { email = Nothing, requested = False, response = Ok () }
    , Effect.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UpdateEmail email ->
            ( { model | email = Just email }, Effect.none )

        Submit ->
            case model.email of
                Just email ->
                    ( { model | requested = True }, Effect.sendCmd (Api.sendResetRequest email Response) )
                Nothing ->
                    ( model, Effect.none )

        Response res ->
            ( { model | response = res }, Effect.none )


view : Model -> View Msg
view model =
    if model.requested then
        case model.response of
            Ok () ->
                { title = "Request sent"
                , body = text "Request sent"
                }
            Err status ->
                { title = "Request failed"
                , body = text (Debug.toString status)
                }
    else
        { title = "Login"
        , body =
            E.column [ E.centerX, E.centerY, E.spacing 8, E.width (E.fill |> E.maximum (16 * 24)) ]
                [ el
                    [ E.paddingXY 0 8
                    , Font.size 28
                    , Font.bold
                    , Region.heading 1
                    ]
                    (E.text "Password Reset")
                , CInput.username []
                    { onChange = UpdateEmail
                    , text = Maybe.withDefault "" model.email
                    , placeholder = Nothing
                    , label = "EMAIL"
                    }
                , CInput.button [] { onPress = Just Submit, label = text "Reset" }
                ]
        }
            
