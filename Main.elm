module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
import Dict as Dict
import Regex exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias ScalableUrl =
    { maxSize : ( Int, Int )
    , urlTemplate : ( String, String )
    }


type Url
    = Url String


scaleUrl : ScalableUrl -> Int -> Maybe Url
scaleUrl { maxSize, urlTemplate } height =
    let
        ( _, maxHeight ) =
            maxSize

        ( a, b ) =
            urlTemplate
    in
        if height <= maxHeight then
            Just (Url (String.concat [ a, toString height, b ]))
        else
            Nothing


type alias Model =
    { urls : List ScalableUrl
    }


init : ( Model, Cmd Msg )
init =
    ( Model []
    , fetch
    )



-- UPDATE


type Msg
    = Fetch
    | NewData (Result Http.Error (List ScalableUrl))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            ( model, fetch )

        NewData (Ok data) ->
            ( Model data, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        f i x =
            case (scaleUrl x 600) of
                Just (Url url) ->
                    img [ src url, style [("width", "600px"), ("height", "450px"), ("position", "absolute"), ("opacity", "0.2")]] []

                _ ->
                    text "no"
    in
        div [] <| List.indexedMap f model.urls



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


fetch : Cmd Msg
fetch =
    let
        url =
            "https://commons.wikimedia.org/w/api.php?origin=*&action=query&format=json&prop=imageinfo&iiprop=url|size|sha1&generator=random&iiurlwidth=100&grnnamespace=6&grnlimit=5"
    in
        Http.send NewData (Http.get url decodeResponse)


mkScalableUrl : ( String, Int, Int ) -> Decoder ScalableUrl
mkScalableUrl ( thumburl, width, height ) =
    let
        result =
            find All (regex "(.*[/-])\\d*(px.*)") thumburl

        errMsg =
            "Invalid Data"
    in
        case result of
            { submatches } :: [] ->
                case submatches of
                    (Just a) :: (Just b) :: [] ->
                        succeed
                            { maxSize = ( width - 1, height - 1 )
                            , urlTemplate = ( a, b )
                            }

                    _ ->
                        fail errMsg

            _ ->
                fail errMsg


decodeResponse : Decoder (List ScalableUrl)
decodeResponse =
    field "query" <|
        field "pages" <|
            Json.Decode.map (List.map Tuple.second << Dict.toList) <|
                dict <|
                    field "imageinfo" <|
                        field "0" <|
                            andThen mkScalableUrl <|
                                map3 (\a b c -> ( a, b, c ))
                                    (field "thumburl" string)
                                    (field "width" int)
                                    (field "height" int)
