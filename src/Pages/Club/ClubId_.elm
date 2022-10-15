module Pages.Club.ClubId_ exposing (Model, Msg, page)

import Api
import Layout exposing (Layout)
import Effect exposing (Effect)
import Html exposing (text)
import Html.Attributes as Attr
import Http
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)

layout : Layout
layout =
    Layout.Navbar



page : Shared.Model -> Route { clubId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init route
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { clubId : String
    , clubInfo : Api.Status Api.ClubInfoResponse
    }


init : Route { clubId : String } -> () -> ( Model, Effect Msg )
init route () =
    ( { clubId = route.params.clubId
      , clubInfo = Api.Loading
      }
    , Effect.fromCmd (Api.getClubInfo route.params.clubId GotResponse)
    )



-- UPDATE


type Msg
    = GotResponse (Result Http.Error Api.ClubInfoResponse)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotResponse (Ok res) ->
            ( { model | clubInfo = Api.Success res }
            , Effect.none
            )

        GotResponse (Err err) ->
            ( { model | clubInfo = Api.Failure err }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
-- We will need to pass in the shared model to check if the person is logged in and can edit.


view : Model -> View Msg
view model =
    { title = "Pages.Club.ClubId_"
    , body =
        [ case model.clubInfo of
            Api.Loading ->
                text "Loading..."

            Api.Failure err ->
                text ("Oh no! Something went wrong: " ++ Debug.toString err)

            Api.Success info ->
                Html.div []
                    [ Html.img
                        [ Attr.src (Maybe.withDefault "" info.profilePictureUrl)
                        , Attr.alt (info.clubName ++ "'s profile picture")
                        ]
                        []
                    , Html.h1 [] [ text info.clubName ]
                    , Html.p [] [ text (Maybe.withDefault "" info.description) ]
                    , Html.em [] [ text info.meetTime ]
                    ]
        ]
    }
