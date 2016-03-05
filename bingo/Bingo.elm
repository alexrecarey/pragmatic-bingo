module Bingo where 

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import String exposing (toUpper, repeat, trimRight)

import StartApp.Simple as StartApp

-- MODEL
newEntry phrase points id = 
    {   phrase = phrase,
        points = points,
        wasSpoken = False,
        id = id
    }


initialModel = 
    { entries  = 
        [ newEntry "Doing Agile the hard way" 200 2
        , newEntry "Future Proof" 100 1
        , newEntry "In the cloud" 300 3
        , newEntry "Iterate" 150 4
        ] 
    }

-- UPDATE
type Action 
    = NoOp 
    | Sort 


update action model =
    case action of
        NoOp ->
            model

        Sort ->
            { model | entries = List.sortBy .points model.entries}


-- VIEW
title message times = 
    message ++ " "
        |> toUpper 
        |> repeat times
        |> trimRight
        |> text


pageHeader = 
    h1 [ ] [title "bingo!" 3]

pageFooter = 
    footer [ ]
        [ a  [ href "https://pragmaticstudio.com"] 
             [ text "The Pragmatic Studio"] 
        ]
 


entryItem entry = 

    li [] 
    [ span [class "phrase"] [text entry.phrase]
    , span [class "points"] [text (toString entry.points)]]


entryList entries = 
    ul [ ] (List.map entryItem entries)  

view address model = 
    div [ id "container" ]
        [ pageHeader
          , entryList model.entries
          , button 
            [ class "sort", onClick address Sort]
            [ text "Sort"]
          , pageFooter]


-- Wire it together
main = 
    StartApp.start
    { model = initialModel
    , view = view
    , update = update
    }