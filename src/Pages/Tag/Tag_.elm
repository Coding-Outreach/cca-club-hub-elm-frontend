module Pages.Tag.Tag_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Route exposing (Route)
import Html
import Page exposing (Page)
import Shared
import View exposing (View)


page : Shared.Model -> Route { tag : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init route
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { tag : String
    }


init : Route { tag : String } -> () -> ( Model, Effect Msg )
init route () =
    ( { tag = route.params.tag }
    , Effect.none
    )



-- UPDATE


type Msg
    = ExampleMsgReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ExampleMsgReplaceMe ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    View.fromString "Pages.Tag.Tag_"
