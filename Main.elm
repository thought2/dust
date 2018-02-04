module Main exposing (..)

import Html exposing (..)
import Html.Keyed as Keyed
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
import Dict as Dict
import Regex exposing (..)
import Random as Random
import Time exposing (..)


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
    , addingTime : Time
    , deletionTime : Time
    }


init : ( Model, Cmd Msg )
init =
    ( { urls = []
      , displayedUrls = Nothing
      , isFetching = False
      , width = 400
      , addingTime = 1
      , deletionTime = 1
      }
    , fetch
    )



-- UPDATE


type Msg
    = Fetch
    | NewData (Result Http.Error (List ScalableUrl))
    | FillDisplayed
    | AddToDisplay Float
    | RemoveFromDisplay Float
    | AddToDisplayRandom
    | RemoveFromDisplayRandom
    | AddingTick Time
    | DeletionTick Time
    | NewAddingTime Time
    | NewDeletionTime Time


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


initDisplay n model =
    case model.displayedUrls of
        Just _ ->
            model

        Nothing ->
            { model
                | displayedUrls = Just (List.take n model.urls)
                , urls = List.drop n model.urls
            }


addAtIndex : Int -> List a -> a -> List a
addAtIndex n xs x =
    List.concat
        [ List.take n xs
        , [ x ]
        , List.drop n xs
        ]


removeAtIndex : Int -> List a -> List a
removeAtIndex n xs =
    List.append (List.take n xs) (List.drop (n + 1) xs)


addToDisplay : Float -> Model -> Model
addToDisplay float model =
    case ( model.urls, model.displayedUrls ) of
        ( x :: xs, Just dUrls ) ->
            let
                index =
                    round (float * toFloat (List.length dUrls))
            in
                { model
                    | displayedUrls = Just <| addAtIndex index dUrls x
                    , urls = List.drop 1 model.urls
                }

        _ ->
            model


maybeStart : Model -> Model
maybeStart model =
    let
        { urls, displayedUrls } =
            model
    in
        if List.length urls >= 15 && not (isJust displayedUrls) then
            initDisplay 5 model
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


removeFromDisplay : Float -> Model -> Model
removeFromDisplay float model =
    case model.displayedUrls of
        Just dUrls ->
            let
                index =
                    round (float * toFloat (List.length dUrls))
            in
                { model
                    | displayedUrls = Just <| removeAtIndex index dUrls
                }

        _ ->
            model


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

        AddToDisplayRandom ->
            ( model, Random.generate AddToDisplay (Random.float 0 1) )

        AddToDisplay float ->
            let
                model_ =
                    addToDisplay float model
            in
                ( model_, maybeFetch model_ )

        RemoveFromDisplayRandom ->
            ( model, Random.generate RemoveFromDisplay (Random.float 0 1) )

        RemoveFromDisplay float ->
            ( removeFromDisplay float model, Cmd.none )

        NewAddingTime t ->
            ( { model | addingTime = t }, Cmd.none )

        NewDeletionTime t ->
            ( { model | deletionTime = t }, Cmd.none )

        AddingTick time ->
            ( model
            , Cmd.batch
                [ Random.generate AddToDisplay (Random.float 0 1)
                , Random.generate NewAddingTime (Random.float 3 5)
                ]
            )

        DeletionTick time ->
            ( model
            , Cmd.batch
                [ Random.generate RemoveFromDisplay (Random.float 0 1)
                , Random.generate NewDeletionTime (Random.float 3 5)
                ]
            )


        _ ->
            ( model, Cmd.none )



-- VIEW


px : a -> String
px n =
    String.append (toString n) "px"


imgStack : List Url -> List (Html Msg)
imgStack xs =
    let
        n =
            List.length xs

        opacity =
            1.0 / (toFloat <| n)

        styles i =
            let
                offset =
                    px <| (toFloat i / toFloat n) * 50.0
            in
                [ ( "width", "600px" )
                , ( "height", "450px" )
                , ( "left", offset )
                , ( "top", offset )
                , ( "border", String.append (px 1) " solid black" )
                , ( "position", "absolute" )
                , ( "opacity", toString opacity )
                ]

        f i (Url url) =
            img [ src url, style (styles i) ] []
    in
        List.indexedMap f xs


imgList : List Url -> Html Msg
imgList xs =
    let
        f url =
            img [ src url, style [ ( "width", px 100 ), ( "height", px 100 ) ] ] []
    in
        Keyed.ol [] <|
            List.map (\(Url url) -> ( url, f url )) xs


view : Model -> Html Msg
view model =
    let
        f (Url url) =
            img [ src url, style [ ( "width", "600px" ), ( "height", "450px" ), ( "position", "absolute" ), ( "opacity", "0.2" ) ] ] []

        g i (Url url) =
            div [] [ text <| String.concat [ (toString i), " ", url ] ]

        menu =
            [ div [ onClick AddToDisplayRandom ] [ text "more" ], div [ onClick RemoveFromDisplayRandom ] [ text " less" ] ]
    in
        case model.displayedUrls of
            Just urls ->
                div [] ([ (imgList urls) ] ++ menu)

            --div [] <| (List.indexedMap g urls) ++ menu ++ [ text (toString <| List.length model.urls) ]
            Nothing ->
                text "loading"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (model.addingTime * second) AddingTick
        , Time.every (model.deletionTime * second) DeletionTick
        ]



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
