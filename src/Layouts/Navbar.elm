module Layouts.Navbar exposing (Model, Msg, Settings, layout)

import Color exposing (..)
import Effect exposing (Effect)
import Element as E exposing (Element)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (class)
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (LoginStatus(..))
import Shared.Msg
import View exposing (View)


type alias Settings =
    {}


layout : Settings -> Shared.Model -> Route () -> Layout Model Msg mainMsg
layout settings shared route =
    Layout.new
        { init = init
        , update = update
        , view = view shared route
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
    = Logout


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Logout ->
            ( model
            , Effect.sendSharedMsg Shared.Msg.Logout
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Route () -> { fromMsg : Msg -> mainMsg, content : View mainMsg, model : Model } -> View mainMsg
view shared route { fromMsg, model, content } =
    { title = content.title
    , body =
        E.column
            [ E.width E.fill
            , E.height E.fill
            , Bg.color mono_700
            , Font.color white
            , Font.size 16
            ]
            [ E.el [ E.centerX, Bg.color red_500, Font.extraBold, E.width E.fill, Font.center ] (E.text "THIS IS CURRENTLY IN BETA, ALL CHANGES WILL BE RESET ON FRIDAY SEPT 15")
            , E.row
                [ Bg.color mono_800
                , E.spacing 16
                , E.paddingXY 32 16
                , E.width E.fill
                , Font.size 20
                ]
                [ E.link
                    [ Font.bold
                    , Border.widthEach { bottom = 4, top = 0, left = 0, right = 0 }
                    , Border.color mono_800
                    , E.mouseOver [ Border.color red_500 ]
                    , E.paddingXY 8 4
                    ]
                    { url = "/", label = E.text "CCA CLUB HUB" }
                , E.link (highlightIfSelected (route.path == Route.Path.Search)) { url = "/search", label = E.text "Search" }
                , E.row [ E.spacing 16, E.alignRight ]
                    (case shared.loginStatus of
                        NotLoggedIn ->
                            [ E.link (highlightIfSelected (route.path == Route.Path.Login)) { url = "/login", label = E.text "Login" } ]

                        LoggedIn { clubId } ->
                            [ E.link (highlightIfSelected (route.path == Route.Path.Club_ClubId_ { clubId = clubId }))
                                { url = "/club/" ++ clubId, label = E.text "Your Club" }
                            , E.link (highlightIfSelected (route.path == Route.Path.Edit)) { url = "/edit", label = E.text "Edit Profile" }
                            , Input.button (highlightIfSelected False) { label = E.text "Logout", onPress = Logout |> fromMsg |> Just }
                            ]
                    )
                ]
            , content.body
            ]
    }


highlightIfSelected : Bool -> List (E.Attribute msg)
highlightIfSelected selected =
    if selected then
        [ Border.widthEach { bottom = 4, top = 0, left = 0, right = 0 }
        , Border.color red_500
        , E.paddingXY 2 4
        ]

    else
        [ Border.widthEach { bottom = 4, top = 0, left = 0, right = 0 }
        , Border.color mono_800
        , E.paddingXY 2 4
        , E.mouseOver [ Border.color red_300 ]
        ]
