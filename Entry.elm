module Entry exposing 
    ( Entry
    , markEntryWithId, sumMarkedPoints
    , viewEntryList, getEntries
    )


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


markEntryWithId : List Entry -> Int -> List Entry
markEntryWithId entries id =
    let
        markEntry e =
            if e.id == id then
                { e | marked = (not e.marked) }
            else
                e
    in
        List.map markEntry entries


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map4 Entry
        (field "id" Decode.int)
        (field "phrase" Decode.string)
        (field "points" Decode.int)
        (succeed False)


getEntries : String -> (Result Http.Error (List Entry) -> msg) -> Cmd msg
getEntries url msg =
    (Decode.list entryDecoder)
        |> Http.get url
        |> Http.send msg


viewEntryItem : (Int -> msg) -> Entry -> Html msg
viewEntryItem msg entry =
    li  [ classList [ ("marked", entry.marked) ], onClick (msg entry.id) ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (toString entry.points) ]
        ]


viewEntryList : (Int -> msg) -> List Entry -> Html msg
viewEntryList msg entries =
    let
        listOfEntries =
            List.map (viewEntryItem msg) entries
    in
        ul [] listOfEntries


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.map .points
        |> List.sum