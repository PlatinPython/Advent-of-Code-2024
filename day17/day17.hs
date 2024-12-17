import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Bits (shiftR, xor)
import Data.Either (fromRight)
import Data.List (tails)
import Text.Parsec (char, newline, sepBy1, string)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (int)

data ProgramState = ProgramState
  { registerA :: Int,
    registerB :: Int,
    registerC :: Int,
    pc :: Int,
    out :: [Int]
  }
  deriving (Show)

type Program = [Int]

part1 :: FilePath -> IO [Int]
part1 file = run . fromRight (error "Failed to parse") <$> parseFromFile parse file

part2 :: FilePath -> IO Int
part2 file = do
  input@(_, program) <- fromRight (error "Failed to parse") <$> parseFromFile parse file
  return . minimum . findA input 0 . tail . reverse $ tails program
  where
    findA :: (ProgramState, Program) -> Int -> [Program] -> [Int]
    findA _ a [] = [a]
    findA input a (xs : xss) = do
      a' <- [a * 8 .. a * 8 + 7]
      guard (run (first (\s -> s {registerA = a'}) input) == xs)
      findA input a' xss

run :: (ProgramState, Program) -> [Int]
run (state, program) = reverse . out $ until (\state -> pc state >= length program) (runInstruction program) state

runInstruction :: Program -> ProgramState -> ProgramState
runInstruction program state = do
  let opcode = program !! pc state
  let operand = program !! (pc state + 1)
  case opcode of
    0 -> state {registerA = registerA state `shiftR` readComboOperand state operand, pc = pc state + 2}
    1 -> state {registerB = registerB state `xor` operand, pc = pc state + 2}
    2 -> state {registerB = readComboOperand state operand `mod` 8, pc = pc state + 2}
    3 ->
      if registerA state == 0
        then
          state {pc = pc state + 2}
        else
          state {pc = operand}
    4 -> state {registerB = registerB state `xor` registerC state, pc = pc state + 2}
    5 -> state {out = (readComboOperand state operand `mod` 8 :) $ out state, pc = pc state + 2}
    6 -> state {registerB = registerA state `shiftR` readComboOperand state operand, pc = pc state + 2}
    7 -> state {registerC = registerA state `shiftR` readComboOperand state operand, pc = pc state + 2}
    _ -> error "Invalid opcode"

readComboOperand :: ProgramState -> Int -> Int
readComboOperand state operand =
  case operand of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 3
    4 -> registerA state
    5 -> registerB state
    6 -> registerC state
    _ -> error "Invalid operand"

parse :: Parser (ProgramState, Program)
parse = do
  string "Register A: "
  registerA <- int
  newline
  string "Register B: "
  registerB <- int
  newline
  string "Register C: "
  registerC <- int
  newline
  newline
  string "Program: "
  program <- sepBy1 int (char ',')
  return (ProgramState registerA registerB registerC 0 [], program)
