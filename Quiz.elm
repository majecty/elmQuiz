module Quiz where

import Debug
import Graphics.Element exposing (Element)
import Graphics.Input.Field as Field
import Keyboard
import Maybe
import Signal
import Signal exposing ((<~), (~))
import String
import Window

import View exposing (xInput, answerInput, view)
import Types exposing (Input, Equation, State)

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
  let setter = (\xInputContent answerContent isEnter eqResult -> {
      xInputContent=xInputContent,
      answerContent=answerContent,
      deltaTime=0,
      isEnter=isEnter,
      equationResult=eqResult
    })
  in
    setter
      <~ xInput.signal
      ~ answerInput.signal
      ~ Keyboard.enter
      ~ evalEquationResult

upstate : Input -> State -> State
upstate {xInputContent, deltaTime, isEnter, equationResult, answerContent} s =
  let numberParseResult = String.toFloat xInputContent.string

      errorMessage =
        if xInputContent.string == ""
           then Nothing
           else case numberParseResult of
              Err message -> Just message
              _ -> Nothing

      parsedXValue = Debug.log "log parsedValue" <| Result.toMaybe numberParseResult
      
  in
  { s | xInputContent <- xInputContent,
        answerContent <- answerContent,
        errorMessage <- errorMessage,
        equationResult <- equationResult,
        parsedXValue <- parsedXValue}

initState : State
initState = {
    xInputContent=Field.noContent,
    answerContent=Field.noContent,
    errorMessage=Nothing,
    equation=["x", "-", "1", "+", "3"],
    equationResult=0,
    parsedXValue = Nothing
  }

applyX : Equation -> Float -> Equation
applyX prevEquation value = 
  let valueToString = toString value
  in
     List.map (\s -> if s == "x" then valueToString else s) prevEquation

signalState : Signal State
signalState = Signal.foldp upstate initState signalInput

main : Signal Element
main =
  Signal.map2 view Window.dimensions signalState
