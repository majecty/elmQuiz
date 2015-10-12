module View (xInput, answerInput, view) where

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

answerInput : Signal.Mailbox Field.Content
answerInput = Signal.mailbox Field.noContent

answerField : Field.Content -> Element
answerField content =
  Field.field Field.defaultStyle (Signal.message answerInput.address) "first operator" content

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
      yValueAdded = List.append showableTokens [ "=", "y" ]
  in
     Element.leftAligned <| Text.fromString
       <| String.append "Problem is : " <| String.concat yValueAdded

showXInputField : State -> Element
showXInputField state =
  let label = Element.leftAligned <| Text.fromString <| "Please Input x"
      textBox = xInputField state.xInputContent
  in
     Element.flow Element.down [ label, textBox ]

defaultHeightOfCharacter : Int
defaultHeightOfCharacter = Element.heightOf <| Element.leftAligned <| Text.fromString "aa"

defaultWidthOfCharacter : Int
defaultWidthOfCharacter = Element.widthOf <| Element.leftAligned <| Text.fromString "aa"

showAnswerField : State -> Element
showAnswerField state =
  let label = Element.leftAligned <| Text.fromString <| "Please write answer."
      textBox = answerField state.answerContent
  in
     Element.flow Element.down [
       label,
       Element.flow Element.right [ 
         Element.size defaultWidthOfCharacter defaultHeightOfCharacter textBox,
         Element.spacer 10 1,
         Element.size defaultWidthOfCharacter defaultHeightOfCharacter textBox
       ]
     ]

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
        showEquation state.equation,
        Element.flow Element.right [
          showXInputField state,
          Element.spacer 10 1,
          showAnswerField state
        ],
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

