module Types where

import Graphics.Element as Element
import Graphics.Input.Field as Field
import Maybe
import Time exposing (Time)

type alias Input = {
  inputContent: Field.Content,
  isEnter: Bool,
  deltaTime: Time,
  equationResult: Float
}

type alias Equation = List String

type alias State = {
  equation: Equation,
  inputContent: Field.Content,
  errorMessage: Maybe String,
  equationResult: Float,
  parsedXValue: Maybe Float
}
