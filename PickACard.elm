import Char exposing (fromCode)
import Html exposing (Html, program, text, div, button, a)
import Html.Attributes exposing (href)
import Time exposing (Time, second)
import Keyboard
import Platform.Cmd
import Random
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)

main = Html.program {
  init = init,
  view = view,
  update = update,
  subscriptions = subscriptions
  }

--------
-- Model
--------

cooldown = 6 * Time.second
hoverDuration = 6 * Time.second
cardLockDuration = 6 * Time.second
hoverSwapDuration = 1/2 * Time.second

type Card = Blue | Red | Gold

-- Create a cyclic rotation of the card colors.
nextCard c = case c of
  Blue -> Red
  Red -> Gold
  Gold -> Blue

-- A mapping of integers to colors so that they can be randomized.
cardMap i = case i of
  0 -> Blue
  1 -> Red
  _ -> Gold

type alias Model = {
  now : Time,
  state : State,
  cdr : Float
  }

type State =
    Ready
  | Hovering Card Time Time
  | CardLocked Card Time
  | Cooldown Time

init = ({ state = Ready, now = 0, cdr = 0}, Platform.Cmd.none)

---------
-- Update
---------

type Msg =
    Tick Time
  | KeyMsg Keyboard.KeyCode
  | RandomCard Card
  | Attack
  | DecrementCdr
  | IncrementCdr

-- Main update function called from htmlProgram. Responds to all messages on
-- the given model.
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Tick now -> (advanceTime now model, Platform.Cmd.none)
  KeyMsg keyCode ->
    if fromCode keyCode == 'W'
    then pressW model
    else (model, Platform.Cmd.none)
  RandomCard card ->
    ({model | state = Hovering card model.now model.now},
     Platform.Cmd.none)
  Attack -> ({model | state = clickMouse model.now model.state},
             Platform.Cmd.none)
  DecrementCdr -> ({ model | cdr = model.cdr - 1 }, Platform.Cmd.none)
  IncrementCdr -> ({ model | cdr = model.cdr + 1 }, Platform.Cmd.none)

-- Responds to the clock tick update by both advancing the now timestamp as
-- well as changing any state that advances as a result of the passing of time.
advanceTime now model = { model | now = now, state = case model.state of
  Ready -> Ready
  Hovering card cardt totalt ->
    if now - totalt >= hoverDuration
    then Cooldown now
    else if now - cardt >= hoverSwapDuration
         then Hovering (nextCard card) now totalt
         else model.state
  CardLocked _ lockedat ->
    if now - lockedat > cardLockDuration
    then Cooldown now
    else model.state
  Cooldown time ->
    if now - time >= cooldown * (1 - model.cdr / 100)
    then Ready
    else model.state }

-- On the pressing of a 'W' advance the state by starting the random pick or
-- locking in a card.
pressW model = case model.state of
  Ready -> (model, makeRandomColor)
  Hovering card _ _ ->
    ({ model | state = CardLocked card model.now}, Platform.Cmd.none)
  x -> (model, Platform.Cmd.none)

-- Send a random number generator request for a color and generate a message.
makeRandomColor =
  Random.generate RandomCard (Random.map cardMap (Random.int 0 2))

-- Register a click (attack) which immediately puts a selected card on cooldown.
clickMouse now state = case state of
  CardLocked _ _ -> Cooldown now
  _ -> state

-------
-- View
-------

-- The main view of the program. Create an HTML view and register mouse clicks
-- and allow for control of the cooldown in a text box.
view : Model -> Html Msg
view model =
  div [] [
    div [] [ Html.text "Welcome to Pick a Card, the Twisted Fate practice app."],
    div [] [ Html.text (
               "Press W to 'cast pick a card'. W again to lock in a card." ++
               "Left mouse click to 'attack' and use the card.") ],
    div [] [ Html.text (
      "This was a one-evening project to try out Elm and I have put no effort "
      ++ "into the look and feel of the page. If there is any interest, I'm "
      ++ "happy to take feature requests and accept pull requests. You can add "
      ++ "them " ),

      Html.a [href "https://github.com/mmachenry/pick-a-card/issues"] [
            Html.text "here"]
    ],

    div [] [
      Html.text "Cooldown reduction",
      button [ onClick DecrementCdr ] [ Html.text "-" ],
      Html.text (toString model.cdr),
      button [ onClick IncrementCdr ] [ Html.text "+" ]
    ],
    div [] [ Html.text ("Current state: " ++ modelToString model) ],
    div [ onClick Attack ] ( drawCard model )
  ]

-- A string description of the current state
modelToString model = case model.state of
  Ready -> "Ready"
  Hovering card t1 t2 -> "Hovering"
  CardLocked card t -> "CardLocked " ++ toString card
  Cooldown t ->
    let secondsLeft = ceiling ((cooldown - model.now + t) / Time.second)
    in "Cooldown " ++ toString secondsLeft

-- Draw an SVG image of a large box with the appropriate color for the state.
drawCard model = case getDisplayColor model.state of
  Nothing -> []
  Just color ->
    [ svg [ viewBox "0 0 100 100" ]
      [ rect [ x "0", y "0", width "20px", height "20px", fill color ] [] ]]

-- Pull the color out of the state if one exists.
getDisplayColor state = case state of
  Hovering c _ _ -> Just (cardColor c)
  CardLocked c _ -> Just (cardColor c)
  _ -> Nothing

-- Stringify the color for use in the SVG output.
cardColor card = case card of
  Blue -> "blue"
  Red -> "red"
  Gold -> "yellow"

----------------
-- Subscriptions
----------------

-- Setup subscriptions to global events that are necessary for updating state.
subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [
  Time.every Time.millisecond Tick,
  Keyboard.downs KeyMsg
  ]

