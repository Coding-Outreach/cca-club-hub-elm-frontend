module Pages.Reset.Reset_ exposing (Model, Msg, page)

import Api
import Effect exposing (Effect)
import Element exposing (text)
import Http
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route { reset : String } -> Page Model Msg
page _ route =
    Page.new
        { init = init route
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Model
    = Loading
    | Exists
    | Invalid


type Msg
    = SegmentationFault
    | IllegalInstruction


fromHttpResult : Result Http.Error () -> Msg
fromHttpResult err =
    case err of
        Ok _ ->
            SegmentationFault

        Err _ ->
            IllegalInstruction


init : Route { reset : String } -> () -> ( Model, Effect Msg )
init route _ =
    ( Loading
    , Effect.sendCmd (Api.checkResetUrl route.params.reset fromHttpResult)
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Effect Msg )
update msg _ =
    case msg of
        SegmentationFault ->
            ( Exists, Effect.none )

        IllegalInstruction ->
            ( Invalid, Effect.none )


view : Model -> View Msg
view model =
    case model of
        Exists ->
            { title = "Bad"
            , body = text "segmentation fault"
            }

        Invalid ->
            { title = "Worse"
            , body = text "illegal instruction"
            }

        Loading ->
            { title = "Loose Screw"
            , body = text "undefined behavior"
            }
