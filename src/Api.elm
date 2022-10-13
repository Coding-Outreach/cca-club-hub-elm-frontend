module Api exposing (..)

import Http
import Json.Decode as D
import Json.Encode as E


type alias LoginResponse =
    { clubId : Int
    , jwtToken : String
    }

backendUrl = ""

doLogin : String -> String -> (Result Http.Error LoginResponse -> msg) -> Cmd msg
doLogin username password msg =
    Http.post
        { body = Http.jsonBody (E.object [ ( "username", E.string username ), ( "password", E.string password ) ])
        , url = backendUrl ++ "/api/auth/login"
        , expect =
            Http.expectJson msg
                (D.map2 LoginResponse
                    (D.field "clubId" D.int)
                    (D.field "jwtToken" D.string)
                )
        }
