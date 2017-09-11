module Terms.Terms.Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Neo as Neo exposing (Address, Wif)
import Regex as Regex
import String.Extra as SE
import List.Extra as LE


type Msg
    = NoOp
    | SetWif String
    | SetPrivateKey String
    | SetAddress String
    | FindKey


type alias Model =
    { privateKey : String
    , wif : String
    , address : String
    , errors : List String
    , message : String
    }


view : Model -> Html Msg
view model =
    div [ style [ ( "padding", "50px" ) ] ]
        [ h1 [] [ text "Neo Key Solver" ]
        , hr [] []
        , div [] (List.map showError model.errors)
        , br [] []
        , div [] [ input [ style [ ( "padding", "10px" ), ( "border", "solid 5px #c9c9c9" ), ( "width", "300px" ) ], onInput SetWif, placeholder "Enter your incorrect NEO WIF here" ] [] ]
          --        , div [] [ input [ style [("padding","10px"),("border","solid 5px #c9c9c9"),("width","300px")], onInput SetPrivateKey, placeholder "Enter your NEO private key here" ] [] ]
        , br [] []
        , div [] [ input [ style [ ( "padding", "10px" ), ( "border", "solid 5px #c9c9c9" ), ( "width", "300px" ) ], onInput SetAddress, placeholder "Enter your NEO wallet address here " ] [] ]
        , br [] []
        , case disabledButton model of
            True ->
                button [ style [ ( "padding", "15px 32px" ), ( "background-color", "#cccccc" ), ( "border", "none" ), ( "text-align", "center" ), ( "text-decoration", "none" ), ( "font-size", "16px" ), ( "color", "white" ), ( "display", "inline-block" ) ] ] [ text "Find" ]

            False ->
                button [ onClick FindKey, style [ ( "padding", "15px 32px" ), ( "background-color", "#4CAF50" ), ( "border", "none" ), ( "text-align", "center" ), ( "text-decoration", "none" ), ( "font-size", "16px" ), ( "color", "white" ), ( "display", "inline-block" ) ] ] [ text "Find" ]
        , br [] []
        , div [ style [("color","green"),("padding","20px")]] [ text model.message]
        ]


showError : String -> Html Msg
showError error =
    div [ style [ ( "color", "red" ) ] ] [ text error ]


disabledButton : Model -> Bool
disabledButton model =
    (String.isEmpty model.address || String.isEmpty model.wif)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetPrivateKey privateKey ->
            ( { model | privateKey = privateKey }, Cmd.none )

        SetWif wif ->
            ( { model | wif = wif }, Cmd.none )

        SetAddress address ->
            ( { model | address = address }, Cmd.none )

        FindKey ->
            let
                addressError =
                    if Neo.isValidAddress model.address then
                        ""
                    else
                        "Sorry the supplied Address is not a valid NEO address"

                wifError1 =
                    if Neo.isValidWif model.wif then
                        "Hey - you supplied a valid WIF already!"
                    else
                        ""

                wifError2 =
                    if (String.length model.wif == 52) then
                        ""
                    else
                        ("Sorry I don't think I can help at this time - your WIF should be 52 chars long - you only have " ++ (toString (String.length model.wif)))

                errors =
                    [ addressError, wifError1, wifError2 ]

                r =
                    findKeyFromBadWif model.wif model.address
            in
                ( { model | errors = errors, message = r }, Cmd.none )


confusions : List ( String, String )
confusions =
    [ ( "g", "q" )
    , ( "p", "n" )
    , ( "m", "n" )
    , ( "y", "z" )
    , ( "u", "v" )
    , ( "a", "o" )
    , ( "l", "1" )
    , ( "b", "6" )
    , ( "0", "o" )
    , ( "o", "0" )
    , ( "g", "9" )
    , ( "q", "9" )
    , ( "I", "l" )
    , ( "T", "I" )
    , ( "D", "O" )
    , ( "C", "G" )
    , ( "L", "I" )
    , ( "M", "N" )
    , ( "P", "B" )
    , ( "F", "R" )
    , ( "U", "O" )
    , ( "U", "V" )
    , ( "E", "F" )
    , ( "V", "W" )
    , ( "X", "Y" )
    , ( "G", "6" )
    , ( "F", "7" )
    , ( "Z", "2" )
    , ( "Q", "2" )
    , ( "O", "0" )
    , ( "B", "8" )
    , ( "D", "0" )
    , ( "S", "5" )
    , ( "S", "8" )
    , ( "Y", "5" )
    , ( "Z", "7" )
    , ( "T", "7" )
    , ( "U", "0" )
    , ( "U", "4" )
    , ( "0", "8" )
    , ( "3", "9" )
    , ( "3", "8" )
    , ( "4", "9" )
    , ( "5", "8" )
    , ( "5", "3" )
    , ( "6", "8" )
    , ( "7", "1" )
    ]


findKeyFromBadWif : String -> Address -> String
findKeyFromBadWif badWif address =
    let
        l =
            List.map (tryConfusion badWif) confusions

        lr =
            List.map (tryConfusion badWif) (List.map (\( a, b ) -> ( b, a )) confusions)

        l2 =
            List.concatMap (trySeqMatches badWif) confusions

        l3 =
            List.concatMap (trySeqMatches badWif) (List.map (\( a, b ) -> ( b, a )) confusions)

        r =
            l ++ lr ++ l2 ++ l3

--        _ =
--            Debug.log ": " r

        f =
            LE.find (\( w, b ) -> b == True) r
    in
        case f of
            Just ( w, b ) ->
                checkAddress address w

            Nothing ->
                "Sorry could not find a Wif matching your address"


checkAddress : Address -> String -> String
checkAddress address w =
    let
        pk =
            Result.withDefault "" (Neo.getHexPrivateKeyFromWIF w)
    in
        case Neo.getAccountFromHexPrivateKey pk of
            Ok ac ->
                if ac.address == address then
                    ("Found WIF: " ++ w ++ " that matches supplied address")
                else
                    "No WIF found that matches your address"

            Err error ->
                ("An error occurred: " ++ error)


tryConfusion : String -> ( String, String ) -> ( String, Bool )
tryConfusion badWif ( a, b ) =
    let
        re =
            Regex.replace Regex.All (Regex.regex ("[" ++ a ++ "]")) (\_ -> b)

        rer =
            re badWif

        res =
            Neo.isValidWif rer
    in
        ( rer, res )


trySeqMatches : String -> ( String, String ) -> List ( String, Bool )
trySeqMatches badWif ( a, b ) =
    let
        clist =
            String.split "" badWif

        ids =
            String.indexes a badWif

        r =
            List.map
                (\i ->
                    changeAt clist
                        i
                        (\x ->
                            if x == a then
                                b
                            else
                                x
                        )
                )
                ids

        r2 =
            List.map (\rx -> Result.withDefault [] rx) r

        r3 =
            List.map (\g -> ( (String.join "" g), Neo.isValidWif (String.join "" g) )) r2
    in
        r3


changeAt : List a -> Int -> (a -> a) -> Result String (List a)
changeAt l i func =
    if i < 0 then
        Err "Bad Index"
    else
        case l of
            [] ->
                Err "Not found"

            x :: xs ->
                if i == 0 then
                    Ok <| func x :: xs
                else
                    Result.map ((::) x) <| changeAt xs (i - 1) func


main : Program Never Model Msg
main =
    Html.program
        { init = ( { privateKey = "", wif = "", address = "", errors = [], message = "" }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
