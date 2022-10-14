module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    , LoginStatus(..)
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Api
import Effect exposing (Effect)
import Json.Decode as D
import Route exposing (Route)
import Shared.Msg exposing (Msg(..))



-- FLAGS


type alias Flags =
    { token : Maybe String
    }


decoder : D.Decoder Flags
decoder =
    D.map Flags (D.maybe (D.field "token" D.string))



-- INIT


type alias Model =
    { loginStatus : LoginStatus
    }


type LoginStatus
    = NotLoggedIn
    | LoggedIn String


init : Result D.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        flags : Flags
        flags =
            flagsResult
                |> Result.withDefault { token = Nothing }

        loginStatus : LoginStatus
        loginStatus =
            case flags.token of
                Nothing ->
                    NotLoggedIn

                Just token ->
                    LoggedIn token
    in
    ( { loginStatus = loginStatus }
    , Effect.none
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Login res ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
