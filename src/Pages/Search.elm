module Pages.Search exposing (Model, Msg, page)

import Api
import Color exposing (..)
import Components.Icon exposing (icon)
import Components.Input as CInput
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
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


layout : Layout
layout =
    Layout.Navbar


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { searchTerm : String
    , clubs : Api.Status Api.ClubListResponse
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { searchTerm = ""
      , clubs = Api.Loading
      }
    , Effect.fromCmd (Api.getClubList GotClubs)
    )



-- UPDATE


type Msg
    = SearchTermChange String
    | GotClubs (Result Http.Error Api.ClubListResponse)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SearchTermChange searchTerm ->
            ( { model | searchTerm = searchTerm }
            , Effect.none
            )

        GotClubs (Err err) ->
            ( { model | clubs = Api.Failure err }, Effect.none )

        GotClubs (Ok list) ->
            ( { model | clubs = Api.Success list }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "search"
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
                [ case model.clubs of
                    Api.Loading ->
                        el [ E.centerX ] (text "Loading...")

                    Api.Failure err ->
                        el [ E.centerX ] (text ("Oh no! Something went wrong: " ++ Debug.toString err))

                    Api.Success list ->
                        Keyed.column
                            [ E.width (E.fill |> E.maximum (16 * 48))
                            , E.centerX
                            ]
                            (List.map viewClubListing (getSearchResults list model.searchTerm))
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


viewClubListing : Api.ClubListItem -> ( String, Element msg )
viewClubListing listing =
    ( listing.id
    , E.link [ E.width E.fill, Bg.color mono_600 ]
        { url = "/club/" ++ listing.id
        , label =
            E.row [ E.padding 16, E.spacing 16 ]
                [ E.image
                    [ E.centerX
                    , Border.rounded 96
                    , E.clip
                    , E.width (E.px 96)
                    , E.height (E.px 96)
                    , E.alignLeft
                    ]
                    { src = listing.profilePictureUrl
                    , description = listing.clubName ++ "'s profile picture"
                    }
                , E.column [ E.spacing 12 ]
                    [ el [ Font.bold, Font.size 32, Region.heading 1 ] (text listing.clubName)
                    , E.row [ Font.color mono_100 ] [ icon "fa-regular fa-clock", el [] (text (" " ++ listing.meetTime)) ]
                    , E.paragraph
                        [ E.paddingEach
                            { top = 8
                            , right = 0
                            , bottom = 0
                            , left = 0
                            }
                        ]
                        [ text listing.description ]
                    ]
                ]
        }
    )
