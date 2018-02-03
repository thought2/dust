module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
import Dict as Dict


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type ScalableUrl
    = ScalableUrl String Int


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
    div [] <| List.map (\(ScalableUrl a b) -> div [] [ text a ]) model.urls



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


decodeResponse : Decoder (List ScalableUrl)
decodeResponse =
    field "query" <|
        field "pages" <|
            Json.Decode.map (List.map Tuple.second << Dict.toList) <|
                dict <|
                    field "imageinfo" <|
                        field "0" <|
                            map2 ScalableUrl (field "thumburl" string) (field "height" int)
