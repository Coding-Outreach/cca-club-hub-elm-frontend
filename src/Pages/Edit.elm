module Pages.Edit exposing (Model, Msg, page)

import Api
import Color exposing (..)
import Components.Input as CInput
import Components.Link exposing (linkStyles)
import Effect exposing (Effect)
import Element as E exposing (el, text)
import Element.Background as Input
import Element.Border as Border
import Element.Font as Font
import File exposing (File)
import File.Select as Select
import Http
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Task
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
    { info : Api.Status Api.ClubInfo
    , token : String
    , editStatus : Maybe (Api.Status ())
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    case shared.loginStatus of
        Shared.NotLoggedIn ->
            ( { info = Api.Loading, token = "", editStatus = Nothing }
            , Effect.pushUrlPath "/"
            )

        Shared.LoggedIn { clubId, token } ->
            ( { info = Api.Loading, token = token, editStatus = Nothing }
            , Effect.fromCmd (Api.getClubInfo clubId GotInitialData)
            )



-- UPDATE


type Field
    = ClubName String
    | ProfilePictureUrl String
    | MeetTime String
    | Description String
    | About String


type Msg
    = GotInitialData (Result Http.Error Api.ClubInfo)
    | FieldUpdate Field
    | ProfilePictureRequested
    | ProfilePictureLoaded File
    | Submit
    | GotResponse (Result Http.Error ())


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotInitialData (Ok info) ->
            ( { model | info = Api.Success info }
            , Effect.none
            )

        GotInitialData (Err err) ->
            ( { model | info = Api.Failure err }
            , Effect.none
            )

        FieldUpdate field ->
            let
                info =
                    model.info

                newInfo =
                    case field of
                        ClubName name ->
                            Api.map (\m -> { m | clubName = String.left 32 name }) info

                        ProfilePictureUrl url ->
                            Api.map (\m -> { m | profilePictureUrl = url }) info

                        Description description ->
                            Api.map (\m -> { m | description = String.left 250 description }) info

                        MeetTime meetTime ->
                            Api.map (\m -> { m | meetTime = String.left 60 meetTime }) info

                        About about ->
                            Api.map (\m -> { m | about = about }) info
            in
            ( { model | info = newInfo }
            , Effect.none
            )

        ProfilePictureRequested ->
            ( model
            , Effect.fromCmd (Select.file [ "image/png", "image/jpeg", "image/webp" ] ProfilePictureLoaded)
            )

        ProfilePictureLoaded file ->
            ( model
            , Effect.fromCmd (Task.perform (FieldUpdate << ProfilePictureUrl) (File.toUrl file))
            )

        Submit ->
            case model.info of
                Api.Success info ->
                    ( { model | editStatus = Just Api.Loading }
                    , Effect.fromCmd (Api.doClubEdit model.token info GotResponse)
                    )

                _ ->
                    ( model, Effect.none )

        GotResponse (Ok _) ->
            ( { model | editStatus = Just (Api.Success ()) }, Effect.none )

        GotResponse (Err err) ->
            ( { model | editStatus = Just (Api.Failure err) }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Login"
    , body =
        case model.info of
            Api.Loading ->
                el [ E.centerX, E.centerY ] (text "Loading...")

            Api.Failure err ->
                el [ E.centerX, E.centerY ] (text ("Something went wrong: " ++ Debug.toString err))

            Api.Success info ->
                E.column [ E.padding 32, E.width (E.fill |> E.maximum (16 * 36)), E.height E.fill, E.centerX, E.spacing 16 ]
                    [ el [ Font.size 24, Font.bold ] (text "General")
                    , CInput.text []
                        { onChange = FieldUpdate << ClubName
                        , text = info.clubName
                        , placeholder = Nothing
                        , label = "DISPLAY NAME"
                        }
                    , E.row [ E.spacing 16 ]
                        [ viewProfilePicture info.profilePictureUrl info.clubName
                        , E.column [ E.width E.fill, E.spacing 16 ]
                            [ E.paragraph [ Font.color mono_300 ] [ text "Upload a profile picture. Dimensions should be between 160x160 to 256x256." ]
                            , CInput.button [] { onPress = Just ProfilePictureRequested, label = text "Upload Image" }
                            ]
                        ]
                    , CInput.text []
                        { onChange = FieldUpdate << MeetTime
                        , text = info.meetTime
                        , placeholder = Nothing
                        , label = "MEET TIME"
                        }
                    , CInput.multiline
                        [ characterLimit 250 info.description ]
                        { onChange = FieldUpdate << Description
                        , text = info.description
                        , placeholder = Nothing
                        , label = "DESCRIPTION"
                        , spellcheck = True
                        }
                    , CInput.multiline
                        [ E.height (E.shrink |> E.minimum (16 * 16)) ]
                        { onChange = FieldUpdate << About
                        , text = info.about
                        , placeholder = Nothing
                        , label = "ABOUT"
                        , spellcheck = True
                        }
                    , E.paragraph []
                        [ E.link linkStyles { url = "https://www.markdownguide.org/", label = text "Markdown" }
                        , text " is accepted!"
                        ]
                    , CInput.button [] { onPress = Just Submit, label = text "Submit Changes" }
                    , Maybe.withDefault (text "") (viewStatusMessage model.editStatus)
                    ]
    }


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


viewProfilePicture : String -> String -> E.Element msg
viewProfilePicture url clubName =
    E.image
        [ Border.rounded 160
        , E.clip
        , E.width (E.px 128)
        , E.height (E.px 128)
        , E.alignLeft
        ]
        { src = url
        , description = clubName ++ "'s profile picture"
        }


viewStatusMessage : Maybe (Api.Status ()) -> Maybe (E.Element msg)
viewStatusMessage =
    Maybe.map
        (\status ->
            let
                color =
                    case status of
                        Api.Loading ->
                            white

                        Api.Success _ ->
                            E.rgb255 0 255 0

                        -- TODO create pallete for greens as well
                        Api.Failure _ ->
                            red_300

                message =
                    case status of
                        Api.Loading ->
                            "Loading..."

                        Api.Success _ ->
                            "Club successfully updated!"

                        Api.Failure (Http.BadStatus 400) ->
                            "You are unauthorized! try relogging in"

                        Api.Failure err ->
                            "Something went wrong: " ++ Debug.toString err
            in
            el [ Font.color color ] (text message)
        )
