module Pages.Search exposing (Model, Msg, page)

import Api
import Color exposing (..)
import Components.Icon exposing (icon)
import Components.Input as CInput
import Components.Rounded
import Effect exposing (Effect)
import Element as E exposing (Element, el, text)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Region as Region
import Fuzzy
import Http
import Layout exposing (Layout)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)
import Components.ClubListing


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (\_ -> Layouts.Navbar { navbar = {} })



-- INIT


type alias Model =
    { searchTerm : String
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { searchTerm = "" }
    , Effect.none
    )



-- UPDATE


type Msg
    = SearchTermChange String


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SearchTermChange searchTerm ->
            ( { model | searchTerm = searchTerm }
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
            [ Input.search
                ([ E.width (E.fill |> E.maximum (16 * 36)), E.centerX ] ++ CInput.inputBoxStyles)
                { onChange = SearchTermChange
                , text = model.searchTerm
                , placeholder = Nothing
                , label =
                    Input.labelAbove
                        [ E.paddingXY 0 8
                        , Font.size 28
                        , Font.bold
                        , Region.heading 1
                        , E.centerX
                        ]
                        (E.text "Search for clubs:")
                }
            , E.column [ E.paddingXY 0 16, E.width E.fill ]
                [ case shared.clubs of
                    Api.Loading ->
                        el [ E.centerX ] (text "Loading...")

                    Api.Failure err ->
                        el [ E.centerX ] (text ("Oh no! Something went wrong: " ++ Api.errorToString err))

                    Api.Success list ->
                        Components.ClubListing.listings (getSearchResults list model.searchTerm)
                ]
            ]
    }


getSearchResults : List Api.ClubListItem -> String -> List Api.ClubListItem
getSearchResults list searchTerm =
    list
        |> List.map (scoreSearchTerm searchTerm)
        |> List.map2 Tuple.pair list
        |> List.sortBy Tuple.second
        |> List.filter (Tuple.second >> (>) 5000)
        |> List.map Tuple.first



-- List.map
-- Tuple.second
-- (List.filter (\a -> Tuple.first a < 1000) (List.map2 Tuple.pair (List.map (scoreSearchTerm searchTerm) list) list))


scoreSearchTerm : String -> Api.ClubListItem -> Int
scoreSearchTerm searchTerm club =
    Fuzzy.match
        []
        [ " " ]
        (String.toLower searchTerm)
        (String.toLower (club.clubName ++ club.description ++ club.meetTime))
        |> .score