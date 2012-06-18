data Colour = Red|Green|Blue

nextColour :: Colour -> Colour
nextColour Red = Green
nextColour Green = Blue
nextColour Blue = Red

data MyBool = MyTrue | MyFalse

---Natural numbers

data Nats = Zero | Succ Nats
