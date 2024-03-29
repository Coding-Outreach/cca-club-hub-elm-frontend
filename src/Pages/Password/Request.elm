module Pages.Password.Request exposing (Model, Msg, page)

import Api
import Components.Input as CInput
import Effect exposing (Effect)
import Element as E exposing (el, text)
import Element.Font as Font
import Element.Region as Region
import Http
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\_ -> Layouts.Navbar { navbar = {} })


type alias Model =
    { email : Maybe String
    , response : Maybe (Result Api.Error ())
    }


type Msg
    = UpdateEmail String
    | Submit
    | Response (Result Api.Error ())


init : () -> ( Model, Effect Msg )
init _ =
    ( { email = Nothing, response = Nothing }
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
                    ( model, Effect.sendCmd (Api.sendResetRequest email Response) )

                Nothing ->
                    ( model, Effect.none )

        Response res ->
            ( { model | response = Just res }, Effect.none )


view : Model -> View Msg
view model =
    { title = "Login"
    , body =
        case model.response of
            Just (Ok _) ->
                el [ E.centerX, E.centerY ] (text "Password reset request sent. Please check your email.")

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
                    , CInput.username []
                        { onChange = UpdateEmail
                        , text = Maybe.withDefault "" model.email
                        , placeholder = Nothing
                        , label = "EMAIL"
                        }
                    , CInput.button [] { onPress = Just Submit, label = text "Send request" }
                    ]
    }
