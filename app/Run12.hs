module Run12 where
import Day12

input = ["KF-sr","OO-vy","start-FP","FP-end","vy-mi","vy-KF","vy-na","start-sr","FP-lh","sr-FP","na-FP","end-KF","na-mi","lh-KF","end-lh","na-start","wp-KF","mi-KF","vy-sr","vy-lh","sr-mi"]

main :: IO ()
main = do
  print (cavePaths1 input)
  print (cavePaths2 input)
