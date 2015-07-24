module Main where

import Html
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy as HL
import Json.Decode as JD
import String
import Window

type alias Model =
    { tasks : List Task
    , field : String
    , uid : Int
    , visibility : String
    }

type alias Task =
    { description : String
    , complated : Bool
    , editing : Bool
    , id : Int
    }

newTask : String -> Int -> Task
newTask desc id =
    { description = desc
    , complated = False
    , editing = False
    , id = id
    }

emptyModel : Model
emptyModel =
    { tasks = []
    , visibility = "All"
    , field = ""
    , uid = 0
    }

type Action
    = NoOp
    | UpdateField String
    | EditingTask Int Bool
    | UpdateTask Int String
    | Add
    | Delete Int
    | DeleteComplate
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String

update : Action -> Model -> Model
update action model =
    case action of
        NoOp -> model
        Add ->
            { model |
                uid <- model.uid + 1,
                field <- "",
                tasks <-
                    if String.isEmpty model.field
                       then model.tasks
                       else model.tasks ++ [newTask model.field model.uid]
            }
        UpdateField str ->
            { model | field <- str }
        EditingTask id isEditing ->
            let updateTask t = if t.id == id then { t | editing <- isEditing } else t
            in
               { model | tasks <- List.map updateTask model.tasks }
        UpdateTask id task ->
            let updateTask t = if t.id == id then { t | description <- task } else t
            in
               { model | tasks <- List.map updateTask model.tasks }
        Delete id ->
            { model | tasks <- List.filter (\t -> t.id /= id) model.tasks }
        DeleteComplate ->
            { model | tasks <- List.filter (not << .complated) model.tasks }
        Check id isComplated ->
            let updateTask t = if t.id == id then { t | complated <- isComplated } else t
            in
               { model | tasks <- List.map updateTask model.tasks }
        CheckAll isComplated ->
            let updateTask t  = { t | complated <- isComplated }
            in
                { model | tasks <- List.map updateTask model.tasks }
        ChangeVisibility visibility ->
            { model | visibility <- visibility }

view : Signal.Address Action -> Model -> Html.Html
view address model =
    Html.div
        [ HA.class "todomvc-wrapper"
        , HA.style [ ("visibility", "hidden") ]
        ]
        [ Html.section
            [ HA.id "todoapp" ]
            [ HL.lazy2 taskEntry address model.field
            , HL.lazy3 taskList address model.visibility model.tasks
            , HL.lazy3 controls address model.visibility model.tasks
            ]
        , infoFooter
        ]

onEnter : Signal.Address a -> a -> Html.Attribute
onEnter address value =
    HE.on "keydown"
        (JD.customDecoder HE.keyCode is13)
        (\_ -> Signal.message address value)

is13 : Int -> Result String ()
is13 code =
    if code == 13 then Result.Ok () else Result.Err "not the right key code"

taskEntry : Signal.Address Action -> String -> Html.Html
taskEntry address task =
    Html.header
        [ HA.id "header" ]
        [ Html.h1 [] [ Html.text "todos" ]
        , Html.input
            [ HA.id "new-todo"
            , HA.placeholder "What needs to be done?"
            , HA.autofocus True
            , HA.value task
            , HA.name "newTodo"
            , HE.on "input" HE.targetValue (Signal.message address << UpdateField)
            , onEnter address Add
            ]
            []
        ]

taskList : Signal.Address Action -> String -> List Task -> Html.Html
taskList address visibility tasks =
    let isVisible todo =
        case visibility of 
            "Complated" -> todo.complated
            "Active" -> not todo.complated
            "All" -> True
        allComplated = List.all .complated tasks
        cssVisibility = if List.isEmpty tasks then "hidden" else "visible"
    in
        Html.section
            [ HA.id "main"
            , HA.style [ ("visibility", cssVisibility) ]
            ]
            [ Html.input
                [ HA.id "toggle-all"
                , HA.type' "checkbox"
                , HA.name "toggle"
                , HA.checked allComplated
                , HE.onClick address (CheckAll (not allComplated))
                ]
                []
            , Html.label
                [ HA.for "toggle-all" ]
                [ Html.text "Mark all as complate" ]
            , Html.ul
                [ HA.id "todo-list" ]
                (List.map (todoItem address) (List.filter isVisible tasks))
            ]

todoItem : Signal.Address Action -> Task -> Html.Html
todoItem address todo =
    Html.li
        [ HA.classList [ ("complated", todo.complated), ("editing", todo.editing) ] ]
        [ Html.div
            [ HA.class "view" ]
            [ Html.input
                [ HA.class "toggle"
                , HA.type' "checkbox"
                , HA.checked todo.complated
                , HE.onClick address (Check todo.id (not todo.complated))
                ]
                []
            , Html.label
                [ HE.onDoubleClick address (EditingTask todo.id True) ]
                [ Html.text todo.description ]
            , Html.button
                [ HA.class "destroy"
                , HE.onClick address (Delete todo.id)
                ]
                []
            ]
        , Html.input
            [ HA.class "edit"
            , HA.value todo.description
            , HA.name "title"
            , HA.id ("todo-"  ++ toString todo.id)
            , HE.on "input" HE.targetValue (Signal.message address << UpdateTask todo.id)
            , HE.onBlur address (EditingTask todo.id False)
            , onEnter address (EditingTask todo.id False)
            ]
            []
        ]

controls : Signal.Address Action -> String -> List Task -> Html.Html
controls address visibility tasks =
    let tasksComplated = List.length (List.filter .complated tasks)
        tasksLeft = List.length tasks - tasksComplated
        item_ = if tasksLeft == 1 then " item" else " items"
    in
        Html.footer
            [ HA.id "footer"
            , HA.hidden (List.isEmpty tasks)
            ]
            [ Html.span
                [ HA.id "todo-count" ]
                [ Html.strong [] [ Html.text (toString tasksLeft) ]
                , Html.text (item_ ++ " left")
                ]
            , Html.ul
                [ HA.id "filters" ]
                [ visibilitySwap address "#/" "All" visibility
                , Html.text " "
                , visibilitySwap address "#/active" "Active" visibility
                , Html.text " "
                , visibilitySwap address "#/complated" "Complated" visibility
                ]
            , Html.button
                [ HA.class "clear-completed"
                , HA.id "clear-completed"
                , HA.hidden (tasksComplated == 0)
                , HE.onClick address DeleteComplate
                ]
                [ Html.text ("Clear complated (" ++ toString tasksComplated ++ ")") ]
            ]

visibilitySwap : Signal.Address Action -> String -> String -> String -> Html.Html
visibilitySwap address uri visibility actualVisibility =
    Html.li
        [ HE.onClick address (ChangeVisibility visibility) ]
        [ Html.a [ HA.href uri, HA.classList [("selected", visibility == actualVisibility)] ]  [ Html.text visibility ] ]

infoFooter : Html.Html
infoFooter =
    Html.footer [ HA.id "info" ]
        [ Html.p [] [ Html.text "Double-click to edit a todo" ]
        , Html.p []
                [ Html.text "Written by "
                , Html.a [ HA.href "https://github.com/evancz" ] [ Html.text "Evan Czaplicki" ]
                ]
        , Html.p []
                [ Html.text "Part of "
                , Html.a [ HA.href "http://todomvc.com" ] [ Html.text "TovoMVC" ]
                ]
        ]

main : Signal.Signal Html.Html
main =
    Signal.map (view actions.address) model

model : Signal.Signal Model
model =
    Signal.foldp update initialModel actions.signal

initialModel =
    Maybe.withDefault emptyModel getStorage

actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp

port focus : Signal.Signal String
port focus =
    let needsFocus act = 
        case act of
            EditingTask id bool -> bool
            _ -> False
        toSelector (EditingTask id _ ) = ("#todo-" ++ toString id)
    in
        actions.signal
            |> Signal.filter needsFocus (EditingTask 0 True)
            |> Signal.map toSelector

port getStorage : Maybe Model

port setStorage : Signal.Signal Model
port setStorage = model
