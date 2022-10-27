module Api exposing (..)

import Http
import Json.Decode as D
import Json.Encode as E


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


doClubEdit : ClubInfo -> (Result Http.Error () -> msg) -> Cmd msg
doClubEdit info msg =
    Http.post
        { body = Http.jsonBody (clubInfoEditEncoder info)
        , url = backendUrl ++ "/edit"
        , expect = Http.expectWhatever msg
        }


maybe : (a -> E.Value) -> Maybe a -> E.Value
maybe encoder =
    Maybe.map encoder >> Maybe.withDefault E.null


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
                [ ( "website", maybe E.string info.socials.website )
                , ( "googleClassroom", maybe E.string info.socials.googleClassroom )
                , ( "discord", maybe E.string info.socials.discord )
                , ( "instagram", maybe E.string info.socials.instagram )
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


type alias ClubListItem =
    { id : String
    , clubName : String
    , meetTime : String
    , description : Maybe String
    , profilePictureUrl : String
    , categories : List String
    }


getClubList : (Result Http.Error ClubListResponse -> msg) -> Cmd msg
getClubList msg =
    Http.get
        { url = backendUrl ++ "/club/list"
        , expect = Http.expectJson msg clubListResponseDecoder
        }


clubListResponseDecoder : D.Decoder ClubListResponse
clubListResponseDecoder =
    D.list
        (D.map6 ClubListItem
            (D.field "id" D.string)
            (D.field "clubName" D.string)
            (D.field "meetTime" D.string)
            (D.field "description" (D.maybe D.string))
            (D.field "profilePictureUrl" D.string)
            (D.field "categories" (D.list D.string))
        )
