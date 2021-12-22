module Day16
    ( bits1, bits2
    ) where

import Util
import Data.List
import Data.List.Split

type Bit = Int
type Bits = [Bit]
type BitCount = Int
type PacketCount = Int
type Version = Int
type TypeId = Int
type Literal = Int
type LengthTypeId = Bit
data Packet = LiteralPacket Version TypeId Literal | OperatorPacket Version TypeId [Packet]
type Operator = [Literal] -> Literal

bits1 :: String -> Int
bits1 s = addVersions packet
  where
    (trailing0s, packet) = readPacket (hexToBits s)
    addVersions (LiteralPacket v _ _) = v
    addVersions (OperatorPacket v _ ps) = v + sum (map addVersions ps)

readAbstract :: (Bits -> a) -> BitCount -> Bits -> (Bits, a)
readAbstract fn n bits = (tail, fn nbits)
  where
    (nbits, tail) = splitAt n bits

parseInt :: Bits -> Int
parseInt bits = inner bits 0
  where
    inner [] i = i
    inner (h : t) i = inner t (i * 2 + h)

readVersion :: Bits -> (Bits, Version)
readVersion = readAbstract parseInt 3

readTypeId = readAbstract parseInt 3

readLiteral bits = (leftOver, parseInt bs)
  where
    (bs, leftOver) = readTo0 bits
    readTo0 (h : t)
      | h == 0 = (four, rest)
      | h == 1 = (four ++ pRead, pRest)
      where
        (four, rest) = splitAt 4 t
        (pRead, pRest) = readTo0 rest

readLengthTypeId = readAbstract head 1

readBitCount = readAbstract parseInt 15

readPacketCount = readAbstract parseInt 11

readPacket :: Bits -> (Bits, Packet)
readPacket bits
  | typeId == 4 = (blitLeftover, LiteralPacket version typeId literal)
  | otherwise = (bopLeftover, OperatorPacket version typeId packets)
  where
    (b2, version) = readVersion bits
    (b3, typeId) = readTypeId b2
    (blitLeftover, literal) = readLiteral b3
    (b4, lengthTypeId) = readLengthTypeId b3
    (bopLeftover, packets) = readPackets lengthTypeId b4

readPackets :: LengthTypeId -> Bits -> (Bits, [Packet])
readPackets 0 bits = (leftOver, inner packetBits)
  where
    (b2, bitCount) = readBitCount bits
    (packetBits, leftOver) = splitAt bitCount b2
    inner [] = []
    inner bs = p : inner ibl
      where
        (ibl, p) = readPacket bs
readPackets 1 bits = inner b2 packetCount
  where
    (b2, packetCount) = readPacketCount bits
    inner bs 0 = (bs, [])
    inner bs pc = (ibl2, p : ps)
      where
         (ibl1, p) = readPacket bs
         (ibl2, ps) = inner ibl1 (pc - 1)

hexToBits :: String -> Bits
hexToBits = concatMap h
  where
    h '0' = [0, 0, 0, 0]
    h '1' = [0, 0, 0, 1]
    h '2' = [0, 0, 1, 0]
    h '3' = [0, 0, 1, 1]
    h '4' = [0, 1, 0, 0]
    h '5' = [0, 1, 0, 1]
    h '6' = [0, 1, 1, 0]
    h '7' = [0, 1, 1, 1]
    h '8' = [1, 0, 0, 0]
    h '9' = [1, 0, 0, 1]
    h 'A' = [1, 0, 1, 0]
    h 'B' = [1, 0, 1, 1]
    h 'C' = [1, 1, 0, 0]
    h 'D' = [1, 1, 0, 1]
    h 'E' = [1, 1, 1, 0]
    h 'F' = [1, 1, 1, 1]

bits2 :: String -> Int
bits2 s = value packet
  where
    (trailing0s, packet) = readPacket (hexToBits s)

operatorFor :: TypeId -> Operator
operatorFor 0 = sum
operatorFor 1 = product
operatorFor 2 = minimum
operatorFor 3 = maximum
operatorFor 5 = booleanOperator (>)
operatorFor 6 = booleanOperator (<)
operatorFor 7 = booleanOperator (==)

booleanOperator :: (Literal -> Literal -> Bool) -> Operator
booleanOperator op [l1, l2] = if op l1 l2 then 1 else 0

value :: Packet -> Literal
value (LiteralPacket _ _ lit) = lit
value (OperatorPacket _ typeId ps) = operatorFor typeId $ map value ps