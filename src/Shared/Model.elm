module Shared.Model exposing (LoginStatus(..), Model)

import Api

type alias Model =
    { loginStatus : LoginStatus
    , clubs : Api.Status Api.ClubListResponse
    }


type LoginStatus
    = NotLoggedIn
    | LoggedIn { token : String, clubId : String }
