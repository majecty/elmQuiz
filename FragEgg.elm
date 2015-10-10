
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
  inputString: Field.Content,
  deltaTime: Time
}
type alias UI = {
  name: Element
}
type alias Equation = List String
type alias State = {
  value: Float,
  equation: Equation,
  inputString: Field.Content
}

signalInput : Signal Input
signalInput =
  (\inputString time ->
    {inputString=inputString, deltaTime=time}) <~ name.signal ~ (Time.fps 30)

upstate : Input -> State -> State
upstate {inputString, deltaTime} s =
  { s | inputString <- inputString }

initState : State
initState = { value=3, inputString=Field.noContent, equation=["1", "*", "3"]}

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
        [ Element.show s.value, showEquation s.equation, nameField s.inputString]
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
