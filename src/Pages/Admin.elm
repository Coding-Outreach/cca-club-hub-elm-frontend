module Pages.Admin exposing (Model, Msg, page)

import Color exposing (..)
import Components.Input as CInput
import Effect exposing (Effect)
import Element as E exposing (el, text)
import Element.Font as Font
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\_ -> Layouts.Navbar { navbar = {} })



-- INIT


type alias Model =
    { adminKey : String
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { adminKey = "" }
    , Effect.none
    )



-- UPDATE


type Msg
    = AdminKeyChange String


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        AdminKeyChange key ->
            ( { model | adminKey = key }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Admin Panel"
    , body =
        E.column [ E.padding 32, E.width (E.fill |> E.maximum (16 * 36)), E.height E.fill, E.centerX, E.spacing 16 ]
            [ CInput.text []
                { onChange = AdminKeyChange
                , text = model.adminKey
                , placeholder = Nothing
                , label = "ENTER ADMIN KEY"
                }
            , E.paragraph [ Font.color mono_300 ]
                [ text "Make sure the admin key is correct. None of the functions will work properly if it isn't correct." ]
            ]
    }
