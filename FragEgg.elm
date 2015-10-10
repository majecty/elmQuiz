
import Graphics.Element exposing (Element)
import Graphics.Element as Element
import Signal
import Window

type alias Input = Float
type alias State = Float

signalInput : Signal Input
signalInput = Signal.constant 3

upstate : Input -> State -> State
upstate _ s = s

initState : State
initState = 3

view : (Int, Int) -> State -> Element
view _ s = Element.show s

main : Signal Element
main =
  Signal.map2 view Window.dimensions
    (Signal.foldp upstate initState signalInput)
