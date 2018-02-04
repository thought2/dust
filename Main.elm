module Main exposing (..)

import Html exposing (..)
import Html.Keyed as Keyed
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (..)
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


type alias Settings =
    { width : Int
    , waitAdd : ( Time, Time )
    , stackN : Int
    , urlBuffer : Int
    }


type alias Model =
    { urls : List Url
    , displayedUrls : List Url
    , isFetching : Bool
    , addingTime : Time
    }


settings : Settings
settings =
    { width = 1000
    , waitAdd = ( 0.0, 5.0 )
    , stackN = 10
    , urlBuffer = 20
    }


init : ( Model, Cmd Msg )
init =
    ( { urls = []
      , displayedUrls = []
      , isFetching = False
      , addingTime = 1
      }
    , fetch
    )



-- UPDATE


type Msg
    = Fetch
    | NewData (Result Http.Error (List ScalableUrl))
    | FillDisplayed
    | AddToDisplay Float
    | AddingTick Time
    | NewAddingTime Time


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


maybeFetch : Model -> Cmd Msg
maybeFetch { urls, isFetching } =
    if List.length urls <= settings.urlBuffer && not isFetching then
        fetch
    else
        Cmd.none


addAtIndex : Int -> List a -> a -> List a
addAtIndex n xs x =
    List.concat
        [ List.take n xs
        , [ x ]
        , List.drop n xs
        ]


addToDisplay : Float -> Model -> Model
addToDisplay float model =
    case ( model.urls, model.displayedUrls ) of
        ( x :: xs, dUrls ) ->
            let
                dUrls_ =
                    List.drop (List.length dUrls - settings.stackN) dUrls

                index =
                    round (float * toFloat (List.length dUrls_))
            in
                { model
                    | displayedUrls = addAtIndex index dUrls_ x
                    , urls = List.drop 1 model.urls
                }

        _ ->
            model


handleNewData : List ScalableUrl -> Model -> Model
handleNewData data model =
    let
        nextUrls =
            List.filterMap (scaleUrl settings.width) data ++ model.urls
    in
        { model | isFetching = False, urls = nextUrls }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            ( { model | isFetching = True }, fetch )

        NewData (Ok data) ->
            let
                model_ =
                    handleNewData data model
            in
                ( model_, maybeFetch model_ )

        NewData _ ->
            ( model, fetch )

        AddToDisplay float ->
            let
                model_ =
                    addToDisplay float model
            in
                ( model_, maybeFetch model_ )

        NewAddingTime t ->
            ( { model | addingTime = t }, Cmd.none )

        AddingTick time ->
            ( model
            , Cmd.batch
                [ Random.generate AddToDisplay (Random.float 0.5 1)
                , Random.generate NewAddingTime
                    (uncurry Random.float <| settings.waitAdd)
                ]
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


px : a -> String
px n =
    String.append (toString n) "px"


imgList : List Url -> Html Msg
imgList xs =
    let
        f i url =
            img [ width 100, src url] []
    in
        Keyed.node "div" [ class "container" ] <|
            List.indexedMap (\i (Url url) -> ( url, f i url )) xs


view : Model -> Html Msg
view model =
    imgList model.displayedUrls



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (model.addingTime * second) AddingTick



-- HTTP


fetch : Cmd Msg
fetch =
    let
        url =
            "https://commons.wikimedia.org/w/api.php?origin=*&action=query&format=json&prop=imageinfo&iiprop=url|size|sha1&generator=random&iiurlwidth=100&grnnamespace=6&grnlimit=20"
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
