module Pages.Edit exposing (Model, Msg, page)

import Api
import Color exposing (..)
import Components.Input exposing (inputBoxStyles)
import Effect exposing (Effect)
import Element as E exposing (el, text)
import Element.Background as Input
import Element.Font as Font
import Element.Input as Input
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
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    Api.Status
        { clubName : String
        , description : String
        , meetTime : String
        , about : String
        }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( Api.Loading
    , case shared.loginStatus of
        Shared.NotLoggedIn ->
            Effect.pushUrlPath "/"

        Shared.LoggedIn { clubId } ->
            Effect.fromCmd (Api.getClubInfo clubId GotInitialData)
    )



-- UPDATE


type Field
    = ClubName String
    | MeetTime String
    | Description String
    | About String


type Msg
    = GotInitialData (Result Http.Error Api.ClubInfoResponse)
    | FieldUpdate Field


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotInitialData (Ok info) ->
            ( Api.Success
                { clubName = info.clubName
                , description = Maybe.withDefault "" info.description
                , meetTime = info.meetTime
                , about = info.about
                }
            , Effect.none
            )

        GotInitialData (Err err) ->
            ( Api.Failure err
            , Effect.none
            )

        FieldUpdate (ClubName name) ->
            ( Api.map (\m -> { m | clubName = String.left 32 name }) model
            , Effect.none
            )

        FieldUpdate (Description description) ->
            ( Api.map (\m -> { m | description = String.left 250 description }) model
            , Effect.none
            )

        FieldUpdate (MeetTime meetTime) ->
            ( Api.map (\m -> { m | meetTime = String.left 60 meetTime }) model
            , Effect.none
            )

        FieldUpdate (About about) ->
            ( Api.map (\m -> { m | about = about }) model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Login"
    , body =
        case model of
            Api.Loading ->
                el [ E.centerX, E.centerY ] (text "Loading...")

            Api.Failure err ->
                el [ E.centerX, E.centerY ] (text ("Something went wrong: " ++ Debug.toString err))

            Api.Success info ->
                E.column [ E.padding 32, E.width (E.fill |> E.maximum (16 * 36)), E.height E.fill, E.centerX, E.spacing 16 ]
                    [ el [ Font.size 24, Font.bold ] (text "General")
                    , Input.text inputBoxStyles
                        { onChange = FieldUpdate << ClubName
                        , text = info.clubName
                        , placeholder = Nothing
                        , label = label "DISPLAY NAME"
                        }
                    , Input.text inputBoxStyles
                        { onChange = FieldUpdate << MeetTime
                        , text = info.meetTime
                        , placeholder = Nothing
                        , label = label "MEET TIME"
                        }
                    , Input.multiline
                        (characterLimit 250 info.description :: inputBoxStyles)
                        { onChange = FieldUpdate << Description
                        , text = info.description
                        , placeholder = Nothing
                        , label = label "DESCRIPTION"
                        , spellcheck = True
                        }
                    , Input.multiline
                        (E.height (E.shrink |> E.minimum (16 * 16)) :: characterLimit 250 info.description :: inputBoxStyles)
                        { onChange = FieldUpdate << About
                        , text = info.about
                        , placeholder = Nothing
                        , label = label "ABOUT"
                        , spellcheck = True
                        }
                    , E.paragraph []
                        [ E.link [ Font.underline, Font.color red_400 ] { url = "https://www.markdownguide.org/", label = text "Markdown" }
                        , text " is accepted!"
                        ]
                    ]
    }


label : String -> Input.Label msg
label name =
    Input.labelAbove [ Font.bold, Font.color mono_400, Font.size 14 ] (text name)


characterLimit : Int -> String -> E.Attribute msg
characterLimit limit current =
    let
        remainingCharacters =
            limit - String.length current

        fontColor =
            if remainingCharacters == 0 then
                red_400

            else
                mono_400
    in
    E.inFront
        (el
            [ E.alignBottom
            , E.alignRight
            , E.padding 8
            , Font.color fontColor
            ]
            (text (String.fromInt remainingCharacters))
        )
