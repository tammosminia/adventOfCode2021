module Run17 where
import Day17

input = "target area: x=56..76, y=-162..-134"
          

main :: IO ()
main = do
  print $ trickShot1 1000 ((56, -134), (76, -162))
  print $ trickShot2 1000 ((56, -134), (76, -162))
