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
    , pfpTooBig : Bool
    , editStatus : Maybe (Api.Status ())
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    case shared.loginStatus of
        Shared.NotLoggedIn ->
            ( { info = Api.Loading, token = "", editStatus = Nothing, pfpTooBig = False }
            , Effect.pushUrlPath "/"
            )

        Shared.LoggedIn { clubId, token } ->
            ( { info = Api.Loading, token = token, editStatus = Nothing, pfpTooBig = False }
            , Effect.fromCmd (Api.getClubInfo clubId GotInitialData)
            )



-- UPDATE


type Social
    = Website String
    | GoogleClassroom String
    | Discord String
    | Instagram String


type Field
    = ClubName String
    | ProfilePictureUrl String
    | MeetTime String
    | Description String
    | About String
    | Social Social


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

        FieldUpdate (Social social) ->
            case model.info of
                Api.Success info ->
                    let
                        socials =
                            info.socials

                        newSocials =
                            case social of
                                Website website ->
                                    { socials | website = Just website }

                                GoogleClassroom gc ->
                                    { socials | googleClassroom = Just gc }

                                Discord dc ->
                                    { socials | discord = Just dc }

                                Instagram ig ->
                                    { socials | instagram = Just ig }

                        newInfo =
                            { info | socials = newSocials }
                    in
                    ( { model | info = Api.Success newInfo }
                    , Effect.fromCmd (Effect.promptToSave True)
                    )

                _ ->
                    ( model, Effect.none )

        FieldUpdate (ProfilePictureUrl url) ->
            let
                size =
                    String.length url
            in
            if size > 64000 then
                ( { model | pfpTooBig = True }, Effect.none )

            else
                ( { model | pfpTooBig = False, info = Api.map (\m -> { m | profilePictureUrl = url }) model.info }
                , Effect.fromCmd (Effect.promptToSave True)
                )

        ProfilePictureRequested ->
            ( model
            , Effect.fromCmd (Select.file [ "image/png", "image/jpeg", "image/webp" ] ProfilePictureLoaded)
            )

        ProfilePictureLoaded file ->
            ( model
            , Effect.fromCmd (Task.perform (FieldUpdate << ProfilePictureUrl) (File.toUrl file))
            )

        FieldUpdate field ->
            let
                info =
                    model.info

                newInfo =
                    case field of
                        ClubName name ->
                            Api.map (\m -> { m | clubName = String.left 32 name }) info

                        Description description ->
                            Api.map (\m -> { m | description = String.left 250 description }) info

                        MeetTime meetTime ->
                            Api.map (\m -> { m | meetTime = String.left 60 meetTime }) info

                        About about ->
                            Api.map (\m -> { m | about = about }) info

                        _ ->
                            info
            in
            ( { model | info = newInfo }
            , Effect.fromCmd (Effect.promptToSave True)
            )

        Submit ->
            case model.info of
                Api.Success info ->
                    ( { model | editStatus = Just Api.Loading }
                    , Effect.batch (List.map Effect.fromCmd [ Effect.promptToSave False, Api.doClubEdit model.token info GotResponse ])
                    )

                _ ->
                    ( model, Effect.fromCmd (Effect.promptToSave False) )

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
                            [ E.paragraph [ Font.color mono_300 ]
                                [ text "Upload a profile picture. Dimensions should be between 160x160 to 256x256."
                                ]
                            , CInput.button [] { onPress = Just ProfilePictureRequested, label = text "Upload Image" }
                            , if model.pfpTooBig then
                                E.paragraph [ Font.color red_300 ]
                                    [ text "Your profile picture is too big."
                                    ]

                              else
                                text ""
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
                    , el [ Font.size 24, Font.bold ] (text "Socials")
                    , CInput.text []
                        { onChange = FieldUpdate << Social << Website
                        , text = Maybe.withDefault "" info.socials.website
                        , placeholder = Nothing
                        , label = "WEBSITE"
                        }
                    , CInput.text []
                        { onChange = FieldUpdate << Social << GoogleClassroom
                        , text = Maybe.withDefault "" info.socials.googleClassroom
                        , placeholder = Nothing
                        , label = "GOOGLE CLASSROOM (put invite code)"
                        }
                    , CInput.text []
                        { onChange = FieldUpdate << Social << Discord
                        , text = Maybe.withDefault "" info.socials.discord
                        , placeholder = Nothing
                        , label = "DISCORD"
                        }
                    , CInput.text []
                        { onChange = FieldUpdate << Social << Instagram
                        , text = Maybe.withDefault "" info.socials.instagram
                        , placeholder = Nothing
                        , label = "INSTAGRAM"
                        }
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
