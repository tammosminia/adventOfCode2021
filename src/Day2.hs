module Day2
    ( forward, down, up, headingFor, aimFor
    ) where

data Direction = Forward | Down | Up deriving (Show, Eq)
type Command = (Direction, Integer)
type Location = (Integer, Integer)  -- horizontal, depth
type Position = (Location, Integer) -- location, aim

forward :: Integer -> Command
forward x = (Forward, x)

down :: Integer -> Command
down x = (Down, x)

up :: Integer -> Command
up x = (Up, x)

headingFor :: [Command] -> Location
headingFor [] = (0, 0)
headingFor ((h, x) : t) = go h
  where
    (thor, tdepth) = headingFor t
    go Forward = (thor + x, tdepth)
    go Down = (thor, tdepth + x)
    go Up = (thor, tdepth - x)

aimFor :: [Command] -> Location
aimFor cs = l
  where (l, aim) = aimForR cs ((0, 0), 0)

aimForR :: [Command] -> Position -> Position
aimForR [] s = s
aimForR ((c, x) : t) ((hor, depth), aim) = aimForR t (go c)
         where
           go Forward = ((hor + x, depth + (aim * x)), aim)
           go Down = ((hor, depth), aim + x)
           go Up = ((hor, depth), aim - x)