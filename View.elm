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

showEquation : Equation -> Element
showEquation equation =
  let showableTokens = List.map hideOperator equation
  in
     Element.leftAligned <| Text.fromString
       <| String.append "Problem is : " <| String.concat showableTokens

showXInputField : State -> Element
showXInputField state =
  let label = Element.leftAligned <| Text.fromString <| "Please Input x"
      textBox = xInputField state.xInputContent
  in
     Element.flow Element.down [ label, textBox ]

stateToElement : State -> Element
stateToElement state =
  let maybeErrorElement = Maybe.map (Element.color Color.red << Element.show) state.errorMessage
      errorElement = Maybe.withDefault Element.empty maybeErrorElement
  in
    Element.flow Element.down
      [
        showEquation state.equation,
        showXInputField state,
        errorElement,
        Element.leftAligned <| Text.fromString <| toString <| state.equationResult
      ]

view : (Int, Int) -> State -> Element
view (w, h) s = 
  let allElements = stateToElement s
      container = Element.container w h Element.middle allElements
      coloredContainer = Element.color Color.brown <| container
  in
     coloredContainer

