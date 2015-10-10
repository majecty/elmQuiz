module Quiz where

import Color
import Graphics.Element exposing (Element)
import Graphics.Element as Element
import Graphics.Input.Field as Field
import Keyboard
import Maybe
import Signal
import Signal exposing ((<~), (~))
import String
import Time
import Time exposing (Time)
import Window

type alias Input = {
  inputString: Field.Content,
  isEnter: Bool,
  deltaTime: Time
}
type alias UI = {
  name: Element
}
type alias Equation = List String
type alias State = {
  equation: Equation,
  inputString: Field.Content,
  errorMessage: Maybe String
}

signalInput : Signal Input
signalInput =
  let setter = (\inputString isEnter -> {
      inputString=inputString,
      deltaTime=0,
      isEnter=isEnter
    })
  in
    setter <~ name.signal ~ Keyboard.enter

upstate : Input -> State -> State
upstate {inputString, deltaTime, isEnter} s =
  let errorMessage =
    if | isEnter && (String.isEmpty inputString.string)
       -> Nothing

       | isEnter
       -> Just "Wrong Answer"

       | otherwise
       -> s.errorMessage
  in
  { s | inputString <- inputString,
        errorMessage <- errorMessage }

initState : State
initState = {
    inputString=Field.noContent,
    errorMessage=Nothing,
    equation=["x", "-", "1", "+", "3"]
  }

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
     Element.show <| String.concat showableTokens

view : (Int, Int) -> State -> Element
view (w, h) s = 
  let allElements = Element.flow Element.down
        [
          showEquation s.equation,
          nameField s.inputString,
          Element.color Color.red <| Element.show s.errorMessage
        ]
      container = Element.container w h Element.middle allElements
      coloredContainer = Element.color Color.brown <| container
  in
     coloredContainer

name : Signal.Mailbox Field.Content
name = Signal.mailbox Field.noContent

nameField : Field.Content -> Element
nameField content =
  Field.field Field.defaultStyle (Signal.message name.address) "Name" content

main : Signal Element
main =
  Signal.map2 view Window.dimensions
    (Signal.foldp upstate initState signalInput)
