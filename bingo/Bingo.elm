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
    | Delete Int
    | Mark Int


update action model =
  case action of
    NoOp ->
      model

    Sort ->
      { model | entries = List.sortBy .points model.entries}

    Delete id ->
      let
        remainingEntries = 
          List.filter (\e -> e.id /= id)
      in
        { model | entries = remainingEntries model.entries }

    Mark id ->
      let
        updateEntries e = 
          if e.id == id 
          then {e | wasSpoken = (not e.wasSpoken)} 
          else e
      in
        {model | entries = List.map updateEntries model.entries}

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
 


entryItem address entry = 
  li 
    [ classList [("highlight", entry.wasSpoken)]
    , onClick address (Mark entry.id)
    ] 
    [ span [class "phrase"] [text entry.phrase]
    , span [class "points"] [text (toString entry.points)]
    , button
      [ class "delete", onClick address (Delete entry.id)]
      [ ]
    ]

totalPoints entries = 
  entries
    |> List.filter .wasSpoken
    |> List.foldl (\e x -> e.points + x) 0


{-
  let
    spokenEntries = List.filter .wasSpoken entries
  in
    List.sum (List.map .points spokenEntries)
-}

totalItem total = 
  li
    [ class "total" ]
    [ span [ class "label"] [ text "Total"]
    , span [ class "points"] [ text (toString total)]
    ]

entryList address entries = 
  let
    entryItems = List.map (entryItem address) entries
    items = entryItems ++ [ totalItem (totalPoints entries)]
  in
    ul [ ] items

view address model = 
  div [ id "container" ]
    [ pageHeader
    , entryList address model.entries
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