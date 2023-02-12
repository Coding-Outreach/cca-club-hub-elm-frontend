module Shared.Model exposing (LoginStatus(..), Model)


type alias Model =
    { loginStatus : LoginStatus
    }


type LoginStatus
    = NotLoggedIn
    | LoggedIn { token : String, clubId : String }
