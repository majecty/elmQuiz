module View (xInput, view) where

import Color
import Graphics.Element exposing (Element)
import Graphics.Element as Element
import Graphics.Input.Field as Field
import List
import Maybe
import String
import Text

import Types exposing (Equation, State)

xInput : Signal.Mailbox Field.Content
xInput = Signal.mailbox Field.noContent

xInputField : Field.Content -> Element
xInputField content =
  Field.field Field.defaultStyle (Signal.message xInput.address) "x = " content

hideOperator : String -> String
hideOperator input = case input of
  "+" -> "ㅁ"
  "-" -> "ㅁ"
  "*" -> "ㅁ"
  "/" -> "ㅁ"
  _ -> input

substituteX : Float -> String -> String
substituteX xValue token = case token of
  "x" -> toString xValue
  _ -> token

showEquation : Equation -> Maybe Float -> Element
showEquation equation xValue =
  let showableTokens = List.map hideOperator equation
      substituted = case xValue of
        Just x -> List.map (substituteX x) showableTokens
        Nothing -> showableTokens
      yValueAdded = List.append substituted [ "=", "y" ]
  in
     Element.leftAligned <| Text.fromString
       <| String.append "Problem is : " <| String.concat yValueAdded

showXInputField : State -> Element
showXInputField state =
  let label = Element.leftAligned <| Text.fromString <| "Please Input x"
      textBox = xInputField state.xInputContent
  in
     Element.flow Element.down [ label, textBox ]

showEquationResult : State -> Element
showEquationResult state =
  let label = Element.leftAligned <| Text.fromString <| "y = "
      yText = Element.leftAligned <| Text.fromString <| toString state.equationResult
  in
     Element.flow Element.right [label, yText]

stateToElement : State -> Element
stateToElement state =
  let maybeErrorElement = Maybe.map (Element.color Color.red << Element.show) state.errorMessage
      errorElement = Maybe.withDefault Element.empty maybeErrorElement
  in
    Element.flow Element.down
      [
        showEquation state.equation state.parsedXValue,
        showXInputField state,
        errorElement,
        showEquationResult state
      ]

view : (Int, Int) -> State -> Element
view (w, h) s = 
  let allElements = stateToElement s
      container = Element.container w h Element.middle allElements
      coloredContainer = Element.color Color.brown <| container
  in
     coloredContainer

