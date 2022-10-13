module Pages.Login exposing (page)

import Html exposing (Html, text)
import Html.Attributes as Attr
import View exposing (View)


page : View msg
page =
    { title = "Pages.Login"
    , body =
        [ Html.div []
            [ Html.div []
                [ Html.h3 [] [ text "Welcome Back!" ]
                , Html.form []
                    [ Html.label [ Attr.for "login-username" ] [ text "Username" ]
                    , Html.input
                        [ Attr.id "login-username"
                        , Attr.name "login-username"
                        , Attr.type_ "text"
                        ] []
                    , Html.label [ Attr.for "login-password" ] [ text "Password" ]
                    , Html.input
                        [ Attr.id "login-password"
                        , Attr.name "login-password"
                        , Attr.type_ "text"
                        ]
                        []
                    , Html.a [ Attr.href "reset-password" ] [ text "Forgot your password?" ]
                    , Html.button [] [ text "Login" ]
                    ]
                ]
            ]
        ]
    }
