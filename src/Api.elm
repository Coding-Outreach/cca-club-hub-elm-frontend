module Api exposing (..)

import Array exposing (Array)
import Http
import Json.Decode as D
import Json.Encode as E
import Jwt.Http


type Status value
    = Loading
    | Success value
    | Failure Http.Error


map : (a -> b) -> Status a -> Status b
map fn status =
    case status of
        Success value ->
            Success (fn value)

        -- We can't do `_ -> status` due to some type issues.
        Loading ->
            Loading

        Failure err ->
            Failure err


withDefault : a -> Status a -> a
withDefault default status =
    case status of
        Success value ->
            value

        _ ->
            default


backendUrl : String
backendUrl =
    "http://localhost:8080/api"


type alias LoginResponse =
    { token : String
    }


doLogin : String -> String -> (Result Http.Error LoginResponse -> msg) -> Cmd msg
doLogin username password msg =
    Http.post
        { body = Http.jsonBody (E.object [ ( "username", E.string username ), ( "password", E.string password ) ])
        , url = backendUrl ++ "/auth/login"
        , expect = Http.expectJson msg loginResponseDecoder
        }


loginResponseDecoder : D.Decoder LoginResponse
loginResponseDecoder =
    D.map LoginResponse (D.field "token" D.string)


type alias ClubInfo =
    { id : String
    , clubName : String
    , meetTime : String
    , description : String
    , about : String
    , profilePictureUrl : String
    , socials : Socials
    , categories : List String
    }


type alias Socials =
    { email : String
    , website : Maybe String
    , googleClassroom : Maybe String
    , discord : Maybe String
    , instagram : Maybe String
    }


getClubInfo : String -> (Result Http.Error ClubInfo -> msg) -> Cmd msg
getClubInfo id msg =
    Http.get
        { url = backendUrl ++ "/club/info/" ++ id
        , expect = Http.expectJson msg clubInfoDecoder
        }


doClubEdit : String -> ClubInfo -> (Result Http.Error () -> msg) -> Cmd msg
doClubEdit token info msg =
    Jwt.Http.post token
        { body = Http.jsonBody (clubInfoEditEncoder info)
        , url = backendUrl ++ "/edit/" ++ info.id
        , expect = Http.expectWhatever msg
        }


maybe : (a -> E.Value) -> Maybe a -> E.Value
maybe encoder =
    Maybe.map encoder >> Maybe.withDefault E.null


nullify : Maybe String -> Maybe String
nullify =
    Maybe.andThen
        (\str ->
            if String.isEmpty str then
                Nothing

            else
                Just str
        )


clubInfoEditEncoder : ClubInfo -> E.Value
clubInfoEditEncoder info =
    E.object
        [ ( "clubName", E.string info.clubName )
        , ( "meetTime", E.string info.meetTime )
        , ( "description", E.string info.description )
        , ( "about", E.string info.about )
        , ( "profilePictureUrl", E.string info.profilePictureUrl )
        , ( "categories", E.list E.string info.categories )
        , ( "socials"
          , E.object
                [ ( "website", maybe E.string (nullify info.socials.website) )
                , ( "googleClassroom", maybe E.string (nullify info.socials.googleClassroom) )
                , ( "discord", maybe E.string (nullify info.socials.discord) )
                , ( "instagram", maybe E.string (nullify info.socials.instagram) )
                ]
          )
        ]


clubInfoDecoder : D.Decoder ClubInfo
clubInfoDecoder =
    D.map8
        ClubInfo
        (D.field "id" D.string)
        (D.field "clubName" D.string)
        (D.field "meetTime" D.string)
        (D.field "description" D.string)
        (D.field "about" D.string)
        (D.field "profilePictureUrl" D.string)
        (D.field "socials" socialsDecoder)
        (D.field "categories" (D.list D.string))


socialsDecoder : D.Decoder Socials
socialsDecoder =
    D.map5 Socials
        (D.field "email" D.string)
        (D.field "website" (D.maybe D.string))
        (D.field "googleClassroom" (D.maybe D.string))
        (D.field "discord" (D.maybe D.string))
        (D.field "instagram" (D.maybe D.string))


type alias ClubListResponse =
    List ClubListItem

type alias FeaturedClubsResponse =
    Array ClubInfo


type alias ClubListItem =
    { id : String
    , clubName : String
    , meetTime : String
    , description : String
    , profilePictureUrl : String
    , categories : List String
    }


clubListItemDecoder : D.Decoder ClubListItem
clubListItemDecoder =
    D.map6 ClubListItem
        (D.field "id" D.string)
        (D.field "clubName" D.string)
        (D.field "meetTime" D.string)
        (D.field "description" D.string)
        (D.field "profilePictureUrl" D.string)
        (D.field "categories" (D.list D.string))


getClubList : (Result Http.Error ClubListResponse -> msg) -> Cmd msg
getClubList msg =
    Http.get
        { url = backendUrl ++ "/club/list"
        , expect = Http.expectJson msg clubListResponseDecoder
        }


clubListResponseDecoder : D.Decoder ClubListResponse
clubListResponseDecoder =
    D.list clubListItemDecoder


getFeaturedClubList : (Result Http.Error FeaturedClubsResponse -> msg) -> Cmd msg
getFeaturedClubList msg =
    Http.get
        { url = backendUrl ++ "/club/list/featured"
        , expect = Http.expectJson msg featuredClubsResponseDecoder
        }


featuredClubsResponseDecoder : D.Decoder FeaturedClubsResponse
featuredClubsResponseDecoder =
    D.array clubInfoDecoder


getCategories : (Result Http.Error (List String) -> msg) -> Cmd msg
getCategories msg =
    Http.get
        { url = backendUrl ++ "/club/categories/list"
        , expect = Http.expectJson msg (D.list D.string)
        }
