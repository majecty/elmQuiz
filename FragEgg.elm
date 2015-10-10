
import Color
import Graphics.Element exposing (Element)
import Graphics.Element as Element
import Graphics.Input.Field as Field
import Signal
import Signal exposing ((<~), (~))
import Time
import Time exposing (Time)
import Window

type alias Input = ( Float, Element, Time)
type alias UI = {
  name: Element
}
type alias State = {
  value: Float,
  ui: UI
}

signalInput : Signal Input
signalInput =
  (\element time -> (3, element, time)) <~ nameField ~ (Time.fps 30)

upstate : Input -> State -> State
upstate (x, nameField, timeDelta) s =
  { s | value <- x,
        ui <- { name=nameField } }

initState : State
initState = { value=3, ui={name=Element.empty}}

view : (Int, Int) -> State -> Element
view (w, h) s = 
  let allElements = Element.flow Element.down [ Element.show s.value, s.ui.name ]
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
