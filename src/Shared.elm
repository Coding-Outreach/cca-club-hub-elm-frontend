module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
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
import Jwt
import Route exposing (Route)
import Route.Path
import Shared.Model exposing (LoginStatus(..))
import Shared.Msg exposing (Msg(..))
import Task
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
    Shared.Model.Model



-- check if token is expired or not.


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

        effect =
            case flags.token of
                Nothing ->
                    Effect.none

                Just token ->
                    Effect.sendCmd (Task.attempt CheckTokenExired (Jwt.checkTokenExpiry token))
    in
    ( { loginStatus = loginStatus, clubs = Api.Loading }
    , Effect.batch [ effect, Effect.sendCmd (Api.getClubList GotClubs) ]
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        CheckTokenExired (Ok expired) ->
            if expired then
                ( { model | loginStatus = NotLoggedIn }, Effect.none )

            else
                ( model, Effect.none )

        CheckTokenExired (Err _) ->
            ( { model | loginStatus = NotLoggedIn }, Effect.none )

        GotClubs (Ok list) ->
            ( { model | clubs = Api.Success list }, Effect.none )

        GotClubs (Err err) ->
            ( { model | clubs = Api.Failure err }, Effect.none )

        Login res ->
            case Token.getClubIdFromToken res.token of
                Ok clubId ->
                    ( { model | loginStatus = LoggedIn { token = res.token, clubId = clubId } }
                    , Effect.batch
                        [ Effect.save { key = "token", value = E.string res.token }
                        , Effect.pushRoute
                            { path = Route.Path.Club_ClubId_ { clubId = clubId }
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
