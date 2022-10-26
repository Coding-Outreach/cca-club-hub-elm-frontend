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
import Dict
import Effect exposing (Effect)
import Json.Decode as D
import Json.Encode as E
import Route exposing (Route)
import Route.Path
import Shared.Msg exposing (Msg(..))
import Token



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
    | LoggedIn { token : String, clubId : String }


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
                    case Token.getClubIdFromToken token of
                        Ok clubId ->
                            LoggedIn { token = token, clubId = clubId }

                        Err _ ->
                            NotLoggedIn
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
            case Token.getClubIdFromToken res.token of
                Ok clubId ->
                    ( { model | loginStatus = LoggedIn { token = res.token, clubId = clubId } }
                    , Effect.batch
                        [ Effect.save { key = "token", value = E.string res.token }
                        , Effect.pushRoute
                            { path = Route.Path.Club__ClubId_ { clubId = clubId }
                            , query = Dict.empty
                            , hash = Nothing
                            }
                        ]
                    )

                Err _ ->
                    ( { model | loginStatus = NotLoggedIn }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
