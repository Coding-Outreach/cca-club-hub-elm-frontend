module Layouts.Navbar exposing (Model, Msg, Settings, layout)

import Color exposing (..)
import Effect exposing (Effect)
import Element as E exposing (Element)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (class)
import Layout exposing (Layout)
import Route exposing (Route)
import Shared
import View exposing (View)


type alias Settings =
    {}


layout : Settings -> Shared.Model -> Route () -> Layout Model Msg mainMsg
layout settings shared route =
    Layout.new
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : { fromMsg : Msg -> mainMsg, content : View mainMsg, model : Model } -> View mainMsg
view { fromMsg, model, content } =
    { title = content.title
    , body =
        E.column
            [ E.width E.fill
            , E.height E.fill
            , Bg.color mono_700
            , Font.color white
            , Font.size 16
            ]
            [ E.row
                [ Bg.color mono_800
                , E.spacing 32
                , E.paddingXY 32 16
                , E.width E.fill
                , Font.size 20
                ]
                [ E.link
                    [ Font.bold
                    , E.mouseOver [ Bg.color red_500 ]
                    , Border.rounded 16
                    , E.paddingXY 8 4
                    ]
                    { url = "/", label = E.text "CCA CLUB HUB" }
                , E.link [] { url = "/about", label = E.text "About" }
                , E.link [] { url = "/login", label = E.text "Login" }
                ]
            , content.body
            ]
    }
