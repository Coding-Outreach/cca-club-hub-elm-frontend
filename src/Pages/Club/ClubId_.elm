module Pages.Club.ClubId_ exposing (Model, Msg, page)

import Api
import Color exposing (..)
import Components.Icon exposing (icon)
import Components.ProfilePicture exposing (profilePicture)
import Components.Rounded
import Effect exposing (Effect)
import Element as E exposing (Element, el, text)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes
import Http
import Layout exposing (Layout)
import Layouts
import Markdown
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Svg
import Svg.Attributes as SvgAttr
import View exposing (View)


page : Shared.Model -> Route { clubId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init route
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\_ -> Layouts.Navbar { navbar = {} })



-- INIT
-- do we need clubId? its already in clubInfo, i guess for the title, but not really
-- TODO refactor Model into a Api.Status Api.ClubInfoResponse


type alias Model =
    { clubId : String
    , clubInfo : Api.Status Api.ClubInfo
    }


init : Route { clubId : String } -> () -> ( Model, Effect Msg )
init route () =
    ( { clubId = route.params.clubId
      , clubInfo = Api.Loading
      }
    , Effect.sendCmd (Api.getClubInfo route.params.clubId GotResponse)
    )



-- UPDATE


type Msg
    = GotResponse (Result Api.Error Api.ClubInfo)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotResponse (Ok res) ->
            ( { model | clubInfo = Api.Success res }
            , Effect.none
            )

        GotResponse (Err err) ->
            ( { model | clubInfo = Api.Failure err }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    let
        title =
            case model.clubInfo of
                Api.Success info ->
                    info.clubName

                _ ->
                    model.clubId
    in
    { title = title
    , body =
        case model.clubInfo of
            Api.Loading ->
                el [ E.centerX, E.centerY ] (text "Loading...")

            Api.Failure err ->
                E.column [ E.centerX, E.centerY ]
                    [ text (Api.errorToString err)
                    , E.link [ Font.color red_300, Font.underline, E.centerX ] { url = "/", label = text "Go Back" }
                    ]

            Api.Success info ->
                E.column [ E.padding 32, E.width E.fill, E.height E.fill ]
                    [ E.column (Components.Rounded.rounded ++ [ E.width E.fill ])
                        [ el [ E.width E.fill, E.height (E.px 128), Bg.color red_100, E.padding 16 ] E.none
                        , E.row
                            [ E.width E.fill
                            , Bg.color mono_600
                            , E.paddingEach
                                { top = 80
                                , right = 32
                                , bottom = 32
                                , left = 32
                                }
                            , E.above (profilePicture info.profilePictureUrl info.clubName)
                            ]
                            [ E.column
                                [ E.width E.fill ]
                                [ E.column [ E.spacing 8 ]
                                    [ el [ Font.bold, Font.size 32, Region.heading 1 ] (text info.clubName)
                                    , E.row [ Font.color mono_100 ] [ icon "fa-regular fa-clock", el [] (text (" " ++ info.meetTime)) ]
                                    ]
                                , E.paragraph
                                    [ E.paddingEach
                                        { top = 16
                                        , right = 0
                                        , bottom = 0
                                        , left = 0
                                        }
                                    ]
                                    [ text info.description ]
                                ]
                            , E.row [ E.alignRight, E.alignTop ]
                                [ viewSocialIcon (Just ("mailto:" ++ info.socials.email)) (icon "fa-regular fa-envelope")
                                , viewSocialIcon info.socials.website (icon "fa-solid fa-earth-americas")
                                , viewSocialIcon info.socials.googleClassroom (E.html (Svg.svg [ SvgAttr.viewBox "0 0 24 24", SvgAttr.height "20", SvgAttr.width "20", SvgAttr.fill "white" ] [ Svg.path [ SvgAttr.d "M1.6367 1.6367C.7322 1.6367 0 2.369 0 3.2734v17.4532c0 .9045.7322 1.6367 1.6367 1.6367h20.7266c.9045 0 1.6367-.7322 1.6367-1.6367V3.2734c0-.9045-.7322-1.6367-1.6367-1.6367H1.6367zm.545 2.1817h19.6367v16.3632h-2.7266v-1.0898h-4.9102v1.0898h-12V3.8184zM12 8.1816c-.9046 0-1.6367.7322-1.6367 1.6368 0 .9045.7321 1.6367 1.6367 1.6367.9046 0 1.6367-.7322 1.6367-1.6367 0-.9046-.7321-1.6368-1.6367-1.6368zm-4.3633 1.9102c-.6773 0-1.2285.5493-1.2285 1.2266 0 .6772.5512 1.2265 1.2285 1.2265.6773 0 1.2266-.5493 1.2266-1.2265 0-.6773-.5493-1.2266-1.2266-1.2266zm8.7266 0c-.6773 0-1.2266.5493-1.2266 1.2266 0 .6772.5493 1.2265 1.2266 1.2265.6773 0 1.2285-.5493 1.2285-1.2265 0-.6773-.5512-1.2266-1.2285-1.2266zM12 12.5449c-1.179 0-2.4128.4012-3.1484 1.0059-.384-.1198-.8043-.1875-1.2149-.1875-1.3136 0-2.7285.695-2.7285 1.5586v.8965h14.1836v-.8965c0-.8637-1.4149-1.5586-2.7285-1.5586-.4106 0-.831.0677-1.2149.1875-.7356-.6047-1.9694-1.0059-3.1484-1.0059Z" ] [] ]))
                                , viewSocialIcon info.socials.discord (icon "fa-brands fa-discord")
                                , viewSocialIcon info.socials.instagram (icon "fa-brands fa-instagram")
                                ]
                            ]
                        ]
                    , E.row [ E.paddingXY 16 32, E.spacing 32, E.width E.fill ]
                        [ E.column
                            [ E.width (E.fillPortion 4) ]
                            [ el [ Font.color mono_300, Font.size 12, Font.bold ] (text "ABOUT")
                            , info.about
                                |> Markdown.toHtml Nothing
                                |> List.map E.html
                                |> E.paragraph [ E.htmlAttribute (Html.Attributes.class "aboutMd") ]
                            ]
                        , E.column
                            [ E.spacing 8
                            , E.alignRight
                            , E.alignTop
                            , E.width (E.fillPortion 1)
                            ]
                            [ el [ Font.color mono_300, Font.size 12, Font.bold ] (text "TAGS")
                            , E.wrappedRow
                                [ E.spacing 8
                                ]
                                (List.map viewCategory info.categories)
                            ]
                        ]
                    ]
    }


viewSocialIcon : Maybe String -> Element Msg -> Element Msg
viewSocialIcon social label =
    case social of
        Just link ->
            E.link
                [ E.mouseOver [ Bg.color mono_500 ]
                , Border.rounded 20
                , E.htmlAttribute (Html.Attributes.style "font-size" "20px")
                , E.padding 8
                ]
                { url = link, label = label }

        Nothing ->
            E.none


viewCategory : String -> Element Msg
viewCategory category =
    E.link
        [ E.paddingXY 12 6
        , Border.rounded 16
        , Bg.color red_100
        , Font.color red_700
        , Font.bold
        , Font.size 12
        , E.mouseOver [ Bg.color red_200 ]
        ]
        { url = "/tag/" ++ category, label = text (String.toUpper category) }
