module Run11 where
import Day11

input = ["5212166716","1567322581","2268461548","3481561744","6248342248","6526667368","5627335775","8124511754","4614137683","4724561156"]

main :: IO ()
main = do
  print (dumboOctopus1 input 100)
  print (dumboOctopus2 input)
