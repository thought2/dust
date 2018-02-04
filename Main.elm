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


scaleUrl : Int -> ScalableUrl -> Maybe Url
scaleUrl width { maxSize, urlTemplate } =
    let
        ( maxWidth, _ ) =
            maxSize

        ( a, b ) =
            urlTemplate
    in
        if width <= maxWidth then
            Just (Url (String.concat [ a, toString width, b ]))
        else
            Nothing


type alias Model =
    { urls : List Url
    , displayedUrls : Maybe (List Url)
    , isFetching : Bool
    , width : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { urls = []
      , displayedUrls = Nothing
      , isFetching = False
      , width = 400
      }
    , fetch
    )



-- UPDATE


type Msg
    = Fetch
    | NewData (Result Http.Error (List ScalableUrl))
    | FillDisplayed
    | AddToDisplay
    | RemoveFromDisplay


getWidth : ( Int, Int ) -> Int
getWidth =
    Tuple.first


maybeFetch : Model -> Cmd Msg
maybeFetch { urls, isFetching } =
    if List.length urls <= 20 && not isFetching then
        fetch
    else
        Cmd.none


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


addToDisplay : Int -> Model -> Model
addToDisplay n model =
    let
        f dUrls =
            { model
                | urls = List.drop n model.urls
                , displayedUrls = Just (dUrls ++ List.take n model.urls)
            }
    in
        case model.displayedUrls of
            Just dUrls ->
                f dUrls

            Nothing ->
                f []


maybeStart : Model -> Model
maybeStart model =
    let
        { urls, displayedUrls } =
            model
    in
        if List.length urls >= 15 && not (isJust displayedUrls) then
            addToDisplay 5 model
        else
            model


handleNewData : List ScalableUrl -> Model -> Model
handleNewData data model =
    let
        nextUrls =
            List.filterMap (scaleUrl model.width) data ++ model.urls

        nextModel =
            { model | isFetching = False, urls = nextUrls }
    in
        nextModel


removeFromDisplay : Model -> Model
removeFromDisplay model =
    let
        { displayedUrls } =
            model

        result =
            Maybe.map List.tail displayedUrls

        nextModel =
            case result of
                Just (Just xs) ->
                    { model | displayedUrls = Just xs }

                _ ->
                    model
    in
        nextModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            ( { model | isFetching = True }, fetch )

        NewData (Ok data) ->
            let
                model_ =
                    maybeStart <| handleNewData data model
            in
                ( model_, maybeFetch model_ )

        NewData _ ->
            ( model, fetch )

        AddToDisplay ->
            let
                model_ =
                    addToDisplay 1 model
            in
                ( model_, maybeFetch model_ )

        RemoveFromDisplay ->
            ( removeFromDisplay model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


imgStack : List Url -> List (Html Msg)
imgStack xs =
    let
        opacity =
            1.0 / (toFloat <| List.length xs)

        styles =
            [ ( "width", "600px" )
            , ( "height", "450px" )
            , ( "position", "absolute" )
            , ( "opacity", toString opacity )
            ]

        f (Url url) =
            img [ src url, style styles ] []
    in
        List.map f xs


view : Model -> Html Msg
view model =
    let
        f (Url url) =
            img [ src url, style [ ( "width", "600px" ), ( "height", "450px" ), ( "position", "absolute" ), ( "opacity", "0.2" ) ] ] []

        g i (Url url) =
            div [] [ text <| String.concat [ (toString i), " ", url ] ]

        menu =
            [ div [ onClick AddToDisplay ] [ text "more" ], div [ onClick RemoveFromDisplay ] [ text " less" ] ]
    in
        case model.displayedUrls of
            Just urls ->
                div [onClick AddToDisplay] (imgStack urls ++ menu)
                --div [] <| (List.indexedMap g urls) ++ menu ++ [ text (toString <| List.length model.urls) ]

            Nothing ->
                text "loading"



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


mkScalableUrl : String -> Int -> Int -> Maybe ScalableUrl
mkScalableUrl thumburl width height =
    let
        result =
            find All (regex "(.*[/-])\\d*(px.*)") thumburl
    in
        case result of
            { submatches } :: [] ->
                case submatches of
                    (Just a) :: (Just b) :: [] ->
                        Just
                            { maxSize = ( width - 1, height - 1 )
                            , urlTemplate = ( a, b )
                            }

                    _ ->
                        Nothing

            _ ->
                Nothing


decodeResponse : Decoder (List ScalableUrl)
decodeResponse =
    at [ "query", "pages" ] <|
        Json.Decode.map (List.filterMap Tuple.second) <|
            keyValuePairs <|
                at [ "imageinfo", "0" ] <|
                    map3 mkScalableUrl
                        (field "thumburl" string)
                        (field "width" int)
                        (field "height" int)
