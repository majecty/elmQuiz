module View  where

import Color
import Graphics.Element exposing (Element)
import Graphics.Element as Element
import Graphics.Input.Field as Field
import List
import Maybe
import String

import Types exposing (Equation, State)

name : Signal.Mailbox Field.Content
name = Signal.mailbox Field.noContent

nameField : Field.Content -> Element
nameField content =
  Field.field Field.defaultStyle (Signal.message name.address) "Name" content

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
     Element.show <| String.append "Problem is : " <| String.concat showableTokens

stateToElement : State -> Element
stateToElement state =
  let maybeErrorElement = Maybe.map (Element.color Color.red << Element.show) state.errorMessage
      errorElement = Maybe.withDefault Element.empty maybeErrorElement
  in
    Element.flow Element.down
      [
        showEquation state.equation,
        nameField state.inputContent,
        errorElement,
        Element.show state.equationResult
      ]

view : (Int, Int) -> State -> Element
view (w, h) s = 
  let allElements = stateToElement s
      container = Element.container w h Element.middle allElements
      coloredContainer = Element.color Color.brown <| container
  in
     coloredContainer

