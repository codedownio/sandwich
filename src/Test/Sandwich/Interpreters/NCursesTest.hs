-- |

module Test.Sandwich.Interpreters.NCursesTest where

import UI.NCurses

main :: IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow

  greenColorID <- newColorID ColorGreen ColorDefault 1
  redColorID <- newColorID ColorRed ColorDefault 2

  updateWindow w $ do
    drawBox Nothing Nothing

    moveCursor 1 1
    setColor greenColorID
    drawString "Hello world!"

    moveCursor 3 1
    setColor redColorID
    drawString "(press q to quit)"

    moveCursor 4 1
    drawLineH (Just (Glyph '-' [])) 999

  render
  waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
  loop = do
    ev <- getEvent w Nothing
    case ev of
      Nothing -> loop
      Just ev' -> if p ev' then return () else loop
