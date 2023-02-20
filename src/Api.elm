module Api exposing (..)

import Array exposing (Array)
import File exposing (File)
import Http
import Json.Decode as D
import Json.Encode as E
import Jwt.Http


type Status value
    = Loading
    | Success value
    | Failure Error


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


type alias ResponseStatusError =
    { status : Int, message : String }


responseStatusErrorDecoder : D.Decoder ResponseStatusError
responseStatusErrorDecoder =
    D.map2 ResponseStatusError (D.field "status" D.int) (D.field "message" D.string)


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus ResponseStatusError
    | BadBody String


errorToString : Error -> String
errorToString e =
    case e of
        BadUrl url ->
            "Bad Url: " ++ url

        BadStatus { status, message } ->
            "Something went wrong: " ++ String.fromInt status ++ " " ++ message

        BadBody error ->
            "Failed to parse response: " ++ error

        Timeout ->
            "Timed Out"

        NetworkError ->
            "Network error"


expectJson : (Result Error a -> msg) -> D.Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ _ body ->
                    case D.decodeString responseStatusErrorDecoder body of
                        Ok value ->
                            Err (BadStatus value)

                        Err err ->
                            Err (BadBody (D.errorToString err))

                Http.GoodStatus_ _ body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (BadBody (D.errorToString err))


expectWhatever : (Result Error () -> msg) -> Http.Expect msg
expectWhatever toMsg =
    Http.expectStringResponse toMsg (resolve (\_ -> Ok ()))


resolve : (String -> Result String a) -> Http.Response String -> Result Error a
resolve toResult response =
    case response of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ _ body ->
            case D.decodeString responseStatusErrorDecoder body of
                Ok value ->
                    Err (BadStatus value)

                Err err ->
                    Err (BadBody (D.errorToString err))

        Http.GoodStatus_ _ body ->
            Result.mapError BadBody (toResult body)


backendUrl : String
backendUrl =
    "http://localhost:8080/"


apiUrl : String
apiUrl =
    backendUrl ++ "api"


type alias LoginResponse =
    { token : String
    }


doLogin : String -> String -> (Result Error LoginResponse -> msg) -> Cmd msg
doLogin username password msg =
    Http.post
        { body = Http.jsonBody (E.object [ ( "username", E.string username ), ( "password", E.string password ) ])
        , url = apiUrl ++ "/auth/login"
        , expect = expectJson msg loginResponseDecoder
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


getClubInfo : String -> (Result Error ClubInfo -> msg) -> Cmd msg
getClubInfo id msg =
    Http.get
        { url = apiUrl ++ "/club/info/" ++ id
        , expect = expectJson msg clubInfoDecoder
        }


doUploadPfpDecoder : D.Decoder String
doUploadPfpDecoder =
    D.field "url" D.string


doUploadPfp : String -> File -> (Result Error String -> msg) -> Cmd msg
doUploadPfp token file toMsg =
    Jwt.Http.put token
        { body = Http.fileBody file
        , url = apiUrl ++ "/edit/pfp"
        , expect = expectJson toMsg doUploadPfpDecoder
        }


doClubEdit : String -> ClubInfo -> (Result Error () -> msg) -> Cmd msg
doClubEdit token info toMsg =
    Jwt.Http.post token
        { body = Http.jsonBody (clubInfoEditEncoder info)
        , url = apiUrl ++ "/edit/info"
        , expect = expectWhatever toMsg
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


getClubList : (Result Error ClubListResponse -> msg) -> Cmd msg
getClubList msg =
    Http.get
        { url = apiUrl ++ "/club/list"
        , expect = expectJson msg clubListResponseDecoder
        }


clubListResponseDecoder : D.Decoder ClubListResponse
clubListResponseDecoder =
    D.list clubListItemDecoder


getFeaturedClubList : (Result Error FeaturedClubsResponse -> msg) -> Cmd msg
getFeaturedClubList msg =
    Http.get
        { url = apiUrl ++ "/club/list/featured"
        , expect = expectJson msg featuredClubsResponseDecoder
        }


featuredClubsResponseDecoder : D.Decoder FeaturedClubsResponse
featuredClubsResponseDecoder =
    D.array clubInfoDecoder


getCategories : (Result Error (List String) -> msg) -> Cmd msg
getCategories msg =
    Http.get
        { url = apiUrl ++ "/club/categories/list"
        , expect = expectJson msg (D.list D.string)
        }


checkResetUrl : String -> (Result Error () -> msg) -> Cmd msg
checkResetUrl uid msg =
    Http.get
        { url = apiUrl ++ "/password/check/" ++ uid
        , expect = expectWhatever msg
        }


sendResetRequest : String -> (Result Error () -> msg) -> Cmd msg
sendResetRequest email msg =
    Http.post
        { url = apiUrl ++ "/password/reset"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "email", E.string email ) ]
                )
        , expect = expectWhatever msg
        }


sendPasswordReset : String -> String -> (Result Error () -> msg) -> Cmd msg
sendPasswordReset uid password msg =
    Http.post
        { url = apiUrl ++ "/password/" ++ uid
        , body =
            Http.jsonBody
                (E.object
                    [ ( "password", E.string password ) ]
                )
        , expect = expectWhatever msg
        }
