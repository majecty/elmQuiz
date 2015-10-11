module Quiz where

import Color
import Debug
import Graphics.Element exposing (Element)
import Graphics.Element as Element
import Graphics.Input.Field as Field
import Keyboard
import Maybe
import Signal
import Signal exposing ((<~), (~))
import String
import Regex
import Time
import Time exposing (Time)
import Window

type alias Input = {
  inputContent: Field.Content,
  isEnter: Bool,
  deltaTime: Time,
  equationResult: Float
}
type alias UI = {
  name: Element
}
type alias Equation = List String
type alias State = {
  equation: Equation,
  inputContent: Field.Content,
  errorMessage: Maybe String,
  equationResult: Float,
  parsedXValue: Maybe Float
}

stateToEquationString : State -> Maybe String
stateToEquationString state = 
  Maybe.map
    (\x -> String.concat <| applyX state.equation x)
    state.parsedXValue

filterMaybeSignal : Signal (Maybe String) -> Signal String
filterMaybeSignal signalMaybeA = Signal.filterMap identity "1" signalMaybeA

port evalEquation : Signal String
port evalEquation =
  let equation = filterMaybeSignal <| stateToEquationString <~ signalState 
  in
     Signal.sampleOn Keyboard.enter equation

port evalEquationResult : Signal Float

signalInput : Signal Input
signalInput =
  let setter = (\inputContent isEnter eqResult -> {
      inputContent=inputContent,
      deltaTime=0,
      isEnter=isEnter,
      equationResult=eqResult
    })
  in
    setter
      <~ name.signal
      ~ Keyboard.enter
      ~ evalEquationResult

upstate : Input -> State -> State
upstate {inputContent, deltaTime, isEnter, equationResult} s =
  let numberParseResult = String.toFloat inputContent.string

      errorMessage = case numberParseResult of
        Err message -> Just message
        _ -> Nothing

      parsedXValue = Debug.log "log parsedValue" <| Result.toMaybe numberParseResult
      
  in
  { s | inputContent <- inputContent,
        errorMessage <- errorMessage,
        equationResult <- equationResult,
        parsedXValue <- parsedXValue}

initState : State
initState = {
    inputContent=Field.noContent,
    errorMessage=Nothing,
    equation=["x", "-", "1", "+", "3"],
    equationResult=0,
    parsedXValue = Nothing
  }

hideOperator : String -> String
hideOperator input = case input of
  "+" -> "ㅁ"
  "-" -> "ㅁ"
  "*" -> "ㅁ"
  "/" -> "ㅁ"
  _ -> input

applyX : Equation -> Float -> Equation
applyX prevEquation value = 
  let valueToString = toString value
  in
     List.map (\s -> if s == "x" then valueToString else s) prevEquation

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
          nameField s.inputContent,
          Element.color Color.red <| Element.show s.errorMessage,
          Element.show s.equationResult
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

signalState : Signal State
signalState = Signal.foldp upstate initState signalInput

main : Signal Element
main =
  Signal.map2 view Window.dimensions signalState
