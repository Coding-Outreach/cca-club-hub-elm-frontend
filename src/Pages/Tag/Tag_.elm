module Pages.Tag.Tag_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Route exposing (Route)
import Page exposing (Page)
import Shared
import View exposing (View)
import Element as E exposing (Element, el, text)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Region as Region
import Api
import Components.ClubListing

page : Shared.Model -> Route { tag : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init route
        , update = update
        , subscriptions = subscriptions
        , view = view shared
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


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Search"
    , body =
        E.column [ E.padding 32, E.width E.fill, E.height E.fill, E.centerX ]
            [ el [ E.paddingXY 0 8
                        , Font.size 28
                        , Font.bold
                        , Region.heading 1
                        , E.centerX
                        ]
                        (E.text "Search for clubs:")
            , E.column [ E.paddingXY 0 16, E.width E.fill ]
                [ case shared.clubs of
                    Api.Loading ->
                        el [ E.centerX ] (text "Loading...")

                    Api.Failure err ->
                        el [ E.centerX ] (text ("Oh no! Something went wrong: " ++ Api.errorToString err))

                    Api.Success list ->
                        Components.ClubListing.listings (filter model.tag list)
                ]
            ]
    }

filter : String -> List Api.ClubListItem -> List Api.ClubListItem
filter tag = List.filter (\item -> List.member tag item.categories)