module Api exposing (..)

import Http
import Json.Decode as D
import Json.Encode as E


type Status value
    = Loading
    | Success value
    | Failure Http.Error


backendUrl : String
backendUrl =
    "https://c6cf53f0-37e5-42b6-b89f-51d11e7d14fe.mock.pstmn.io/api"


type alias LoginResponse =
    { clubId : String
    , token : String
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
    D.map2 LoginResponse (D.field "clubId" D.string) (D.field "token" D.string)


type alias ClubInfoResponse =
    { id: String
    , clubName: String
    , email: String
    , meetTime: String
    , description: Maybe String
    , profilePictureUrl: Maybe String
    , socials: List Social
    , categories: List String
    }

type alias Social =
    { name: String
    , link: String
    }

getClubInfo : String -> (Result Http.Error ClubInfoResponse -> msg) -> Cmd msg
getClubInfo id msg =
    Http.get
        { url = backendUrl ++ "/club/info/" ++ id
        , expect = Http.expectJson msg clubInfoResponseDecoder
        }


clubInfoResponseDecoder : D.Decoder ClubInfoResponse
clubInfoResponseDecoder =
    D.map8
        ClubInfoResponse
        (D.field "id" D.string)
        (D.field "clubName" D.string)
        (D.field "email" D.string)
        (D.field "meetTime" D.string)
        (D.field "description" (D.maybe D.string))
        (D.field "profilePictureUrl" (D.maybe D.string))
        (D.field "socials" (D.list socialDecoder))
        (D.field "categories" (D.list D.string))


socialDecoder : D.Decoder Social
socialDecoder = D.map2 Social (D.field "name" D.string) (D.field "link" D.string)