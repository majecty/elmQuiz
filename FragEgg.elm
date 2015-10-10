
import Color
import Graphics.Element exposing (Element)
import Graphics.Element as Element
import Graphics.Input.Field as Field
import Signal
import Signal exposing ((<~), (~))
import String
import Time
import Time exposing (Time)
import Window

type alias Input = {
  inputField: Element,
  deltaTime: Time
}
type alias UI = {
  name: Element
}
type alias Equation = List String
type alias State = {
  value: Float,
  equation: Equation,
  ui: UI
}

signalInput : Signal Input
signalInput =
  (\element time -> {inputField=element, deltaTime=time}) <~ nameField ~ (Time.fps 30)

upstate : Input -> State -> State
upstate {inputField, deltaTime} s =
  { s | ui <- { name=inputField } }

initState : State
initState = { value=3, ui={name=Element.empty}, equation=["1", "*", "3"]}

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
        [ Element.show s.value, showEquation s.equation, s.ui.name ]
      container = Element.container w h Element.middle allElements
      coloredContainer = Element.color Color.brown <| container
  in
     coloredContainer

name : Signal.Mailbox Field.Content
name = Signal.mailbox Field.noContent

nameField : Signal Element
nameField =
  Field.field Field.defaultStyle (Signal.message name.address) "Name" <~ name.signal

main : Signal Element
main =
  Signal.map2 view Window.dimensions
    (Signal.foldp upstate initState signalInput)
