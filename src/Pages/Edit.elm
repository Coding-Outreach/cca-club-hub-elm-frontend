module Pages.Edit exposing (Model, Msg, page)

import Api
import Color exposing (..)
import Components.Input as CInput
import Components.Link exposing (linkStyles)
import Effect exposing (Effect)
import Element as E exposing (el, text)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Html
import Html.Attributes as Attr
import Html.Events
import Http
import Json.Decode as D
import Layout exposing (Layout)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model exposing (LoginStatus(..))
import Task
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\_ -> Layouts.Navbar { navbar = {} })



-- INIT


type alias Model =
    { info : Api.Status Api.ClubInfo
    , categoryField : String
    , categories : Api.Status (List String)
    , token : String
    , badPfp : Maybe Api.Error
    , categoryInvalid : Bool
    , editStatus : Maybe (Api.Status ())
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    let
        initial =
            { info = Api.Loading
            , token = ""
            , categories = Api.Loading
            , categoryField = ""
            , editStatus = Nothing
            , badPfp = Nothing
            , categoryInvalid = False
            }
    in
    case shared.loginStatus of
        Shared.Model.NotLoggedIn ->
            ( initial
            , Effect.pushUrlPath "/login"
            )

        Shared.Model.LoggedIn { clubId, token } ->
            ( { initial | token = token }
            , Effect.batch [ Effect.sendCmd (Api.getCategories GotCategories), Effect.sendCmd (Api.getClubInfo clubId GotInitialData) ]
            )



-- UPDATE


type Social
    = Website String
    | GoogleClassroom String
    | Discord String
    | Instagram String


type Field
    = ClubName String
    | MeetTime String
    | Description String
    | About String
    | Social Social
    | Category String


type Msg
    = GotInitialData (Result Api.Error Api.ClubInfo)
    | GotCategories (Result Api.Error (List String))
    | FieldUpdate Field
    | ProfilePictureRequested
    | ProfilePictureLoaded File
    | ProfilePictureUpdated (Result Api.Error String)
    | Submit
    | GotSubmit (Result Api.Error ())
    | AddCategory
    | DeleteCategory String



-- TODO honestly, it might be better to have one big case statement at the very top.


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

        GotCategories (Ok list) ->
            ( { model | categories = Api.Success list }, Effect.none )

        GotCategories (Err err) ->
            ( { model | categories = Api.Failure err }, Effect.none )

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
                    , Effect.warnUnsavedChanges True
                    )

                _ ->
                    ( model, Effect.none )

        FieldUpdate (Category category) ->
            ( { model | categoryField = category, categoryInvalid = False }, Effect.none )

        AddCategory ->
            case model.info of
                Api.Success info ->
                    let
                        valid =
                            validCategory model.categories info.categories model.categoryField

                        newInfo =
                            case valid of
                                Just category ->
                                    { info | categories = info.categories ++ [ category ] }

                                Nothing ->
                                    info

                        newCategoryField =
                            case valid of
                                Just _ ->
                                    ""

                                Nothing ->
                                    model.categoryField

                        newCategoryInvalid =
                            case valid of
                                Just _ ->
                                    False

                                Nothing ->
                                    True
                    in
                    ( { model
                        | info = Api.Success newInfo
                        , categoryField = newCategoryField
                        , categoryInvalid = newCategoryInvalid
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        DeleteCategory category ->
            case model.info of
                Api.Success info ->
                    let
                        newInfo =
                            { info | categories = List.filter ((/=) category) info.categories }
                    in
                    ( { model | info = Api.Success newInfo }, Effect.none )

                _ ->
                    ( model, Effect.none )

        ProfilePictureRequested ->
            ( model
            , Effect.sendCmd (Select.file [ "image/*" ] ProfilePictureLoaded)
            )

        ProfilePictureLoaded file ->
            ( model
            , Effect.sendCmd (Api.doUploadPfp model.token file ProfilePictureUpdated)
            )

        ProfilePictureUpdated (Ok url) ->
            case model.info of
                Api.Success info ->
                    let
                        newInfo =
                            { info | profilePictureUrl = url }
                    in
                    ( { model | info = Api.Success newInfo, badPfp = Nothing }, Effect.none )

                _ ->
                    ( model, Effect.none )

        ProfilePictureUpdated (Err err) ->
            ( { model | badPfp = Just err }, Effect.none )

        FieldUpdate field ->
            case model.info of
                Api.Success info ->
                    let
                        newInfo =
                            case field of
                                ClubName name ->
                                    { info | clubName = String.left 32 name }

                                Description description ->
                                    { info | description = String.left 250 description }

                                MeetTime meetTime ->
                                    { info | meetTime = String.left 60 meetTime }

                                About about ->
                                    { info | about = about }

                                _ ->
                                    info
                    in
                    ( { model | info = Api.Success newInfo }
                    , Effect.warnUnsavedChanges True
                    )

                _ ->
                    ( model, Effect.none )

        Submit ->
            case model.info of
                Api.Success info ->
                    ( { model | editStatus = Just Api.Loading }
                    , Effect.sendCmd (Api.doClubEdit model.token info GotSubmit)
                    )

                _ ->
                    ( model, Effect.none )

        GotSubmit (Ok _) ->
            ( { model | editStatus = Just (Api.Success ()) }, Effect.warnUnsavedChanges False )

        GotSubmit (Err err) ->
            ( { model | editStatus = Just (Api.Failure err) }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Edit"
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
                            , case model.badPfp of
                                Just err ->
                                    E.paragraph [ Font.color red_300 ] [ text (Api.errorToString err) ]

                                Nothing ->
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
                    , E.paragraph [ Font.color mono_300 ]
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
                        , label = "GOOGLE CLASSROOM"
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
                    , el [ Font.size 24, Font.bold ] (text "Tags")
                    , E.paragraph [ Font.color mono_300 ] [ text "Press enter to add a tag. Click one of the categories to remove them." ]
                    , CInput.text
                        [ E.htmlAttribute (Attr.list "categories")
                        , onEnter AddCategory
                        , if model.categoryInvalid then
                            Font.color red_400

                          else
                            Font.color white
                        ]
                        { onChange = FieldUpdate << Category
                        , text = model.categoryField
                        , placeholder = Nothing
                        , label = ""
                        }
                    , E.wrappedRow [ E.spacing 8 ] (List.map viewCategory info.categories)
                    , CInput.button [] { onPress = Just Submit, label = text "Submit Changes" }
                    , Maybe.withDefault (text "") (viewStatusMessage model.editStatus)
                    , categoriesDatalist model.categories info.categories
                    ]
    }


viewCategory : String -> E.Element Msg
viewCategory category =
    Input.button
        [ E.paddingXY 12 6
        , E.spacing 8
        , Border.rounded 16
        , Bg.color red_100
        , Font.color red_700
        , Font.bold
        , Font.size 12
        , E.mouseOver [ Bg.color red_200 ]
        ]
        { onPress = Just (DeleteCategory category)
        , label = text (String.toUpper category)
        }


validCategory : Api.Status (List String) -> List String -> String -> Maybe String
validCategory list current value =
    list
        |> Api.withDefault []
        |> List.filter (\v -> List.member v current |> not)
        |> List.filter (\v -> String.toLower v == String.toLower value)
        |> List.head


categoriesDatalist : Api.Status (List String) -> List String -> E.Element msg
categoriesDatalist list current =
    list
        |> Api.withDefault []
        -- Filter for tags that haven't been used yet
        |> List.filter (\v -> List.member v current |> not)
        -- Create Option elements
        |> List.map (\v -> Html.option [ Attr.value v ] [])
        -- Create Datalist element
        |> Html.datalist [ Attr.id "categories" ]
        -- Turn into elm-ui elements
        |> E.html


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
        { src = Api.backendUrl ++ url
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

                        Api.Failure err ->
                            Api.errorToString err
            in
            el [ Font.color color ] (text message)
        )


onEnter : msg -> E.Attribute msg
onEnter msg =
    E.htmlAttribute
        (Html.Events.on "keyup"
            (D.field "key" D.string
                |> D.andThen
                    (\key ->
                        if key == "Enter" then
                            D.succeed msg

                        else
                            D.fail "Not the enter key"
                    )
            )
        )
