{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, BangPatterns, NoMonomorphismRestriction #-}

-- An emulator for DCPU-16, the 16-bit virtual machine designed by Notch for 0x10c.
-- This is based on the specification version 1.1.
-- Added instructions:
-- DIE. Exits with the given code.
-- INP. Accepts a max length in A and buffer address as the argument, and writes bytes from standard input.
-- OUT. Writes the given byte to stdout.
-- HEX. Writes the given value as a hex number.

import Data.Word
import Data.Char
import Data.Bits
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Maybe
import Data.List

import System.IO
import System.Environment (getArgs)
import Data.Binary.Put
import qualified Data.ByteString.Lazy as B

import Text.Printf

import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many, optional)

import Numeric (readHex)

import Debug.Trace


-- Phases:
-- Read input file.
-- Parse into internal rep.
-- Assemble into binary rep and log label locations.
-- Write completed binary rep, resolving labels from the dictionary as it goes.

data Arg = Reg Word16 | RegAddr Word16 | LitAddr Word16 | LitAndReg Word16 Word16 | Lit Word16 | LabelUse String | LabelAddr String | Peek | Push | Pop | SP | PC | O | Nil
  deriving (Show)

data Asm = LabelDef String
         | Instr String Arg Arg
         | Blank
         | LitWord Word16
         | LitLabel String
         | LitASCII String
         | Symbol String [Word16]
         | VarLabel String String -- for setting the value of an assembler .var to a label
         | VarLit String Word16 -- for setting the value of an assembler .var to a literal
         | Array Word16 -- array with given size
         | ArrayLabel Word16 String -- array with given size and label for a symbol holding its data
  deriving (Show)


showHex :: Word16 -> String
showHex x = printf "%04x" x

-- parses a label: begins with a letter or underscore, continues with a letter, number or underscore.
pl = (:) <$> (oneOf "_-" <|> letter) <*> many (oneOf "_-" <|> alphaNum)

parser = many $ do
  x <- try pComment <|> pDirective <|> pLine
  spaces
  return x

pWS = skipMany (oneOf " \t\r")

pComment = pWS *> char ';' *> manyTill anyChar (try newline) *> pure Blank


pDirective = do
    char '.'
    x <- try pASCII <|> pArray <|> pWord <|> pSymbol <|> pVar
    spaces
    return x
  where pASCII = do
          string "ascii" >> space >> pWS
          char '"'
          x <- many (noneOf "\"")
          char '"'
          return $ LitASCII x
        pArray = do
          string "array" >> space >> pWS
          len <- read <$> many1 digit
          space >> pWS
          mlabel <- optionMaybe pl
          return $ case mlabel of 
                     Just label -> ArrayLabel len label
                     Nothing    -> Array len
        pWord = do
          string "word" >> space >> pWS
          pNumber <|> pLitLabel
        pNumber = do
          Lit word <- pLit
          return $ LitWord word
        pLitLabel = LitLabel <$> pl
        pSymbol = do
          string "sym"
          space >> pWS
          name <- pl
          space >> pWS
          values_ <- sepBy1 pLit (many1 (oneOf " \t\r"))
          let values = map (\(Lit x) -> x) values_
          return $ Symbol name values
        pVar = do
          string "var"
          space >> pWS
          name <- pl
          space >> pWS
          value_ <- pNumber <|> pLitLabel
          case value_ of
            LitWord w -> return $ VarLit name w
            LitLabel label -> return $ VarLabel name label


pLine = try pLabel <|> pInstr

pLabel = do
  pWS
  char ':'
  LabelDef <$> pl

pInstr = try pRegInstr <|> pExtInstr

pRegInstr = do
  pWS
  op <- pOpcode
  space
  pWS
  a <- pArg
  char ','
  pWS
  b <- pArg
  pWS
  optional (try pComment)
  return $ Instr op a b

pExtInstr = do
  pWS
  op <- pExtOpcode
  space
  pWS
  a <- pArg
  optional (try pComment)
  return $ Instr op a Nil


pOpcode =  try (string "SET")
       <|> try (string "ADD")
       <|> try (string "SUB")
       <|> try (string "MUL")
       <|> try (string "DIV")
       <|> try (string "MOD")
       <|> try (string "SHL")
       <|> try (string "SHR")
       <|> try (string "AND")
       <|> try (string "BOR")
       <|> try (string "XOR")
       <|> try (string "IFE")
       <|> try (string "IFN")
       <|> try (string "IFG")
       <|> string "IFB"

pExtOpcode = string "JSR" <|> string "DIE" <|> string "INP" <|> string "OUT" <|> string "HEX" <|> string "BRK" <|> string "RND"


-- several different kinds of arguments
pArg = try pReg <|> try pRegAddr <|> try pLitAndReg <|> try pLitAddr <|> try pPeek <|> try pPush <|> try pPop <|> try pSP <|> try pPC <|> try pO <|> try pLit <|> pLabelUse

pReg = do
  r <- oneOf "ABCXYZIJ"
  return $ Reg (regMap ! r)

pRegAddr = do
  Reg r <- brackets pReg
  return $ RegAddr r

brackets = between (char '[') (char ']')


pLitAndReg = do
  char '['
  spaces
  Lit lit <- pLit
  spaces
  char '+'
  spaces
  Reg reg <- pReg
  spaces
  char ']'
  return $ LitAndReg lit reg

pLitAddr = do
  content <- brackets (pLit <|> pLabelUse)
  case content of
    LabelUse label -> return $ LabelAddr label
    Lit lit -> return $ LitAddr lit

pPeek = string "PEEK" >> return Peek
pPush = string "PUSH" >> return Push
pPop  = string "POP"  >> return Pop
pSP   = string "SP"   >> return SP
pPC   = string "PC"   >> return PC
pO    = char 'O'      >> return O

pLit = do
  hex <- not.null <$> option "" (try $ string "0x")
  ds <- many1 (if hex then hexDigit else digit)
  let val = case hex of
        False -> read ds
        True  -> case readHex ds of
            [(x, "")] -> x
            _         -> error "Parse error in hex string."
  return $ Lit val

pLabelUse = LabelUse <$> pl


regMap :: Map Char Word16
regMap = M.fromList $ zip "ABCXYZIJ" [0..]



parseFile :: String -> IO [Asm]
parseFile s = do
  content <- B.readFile s
  let res = parse parser s content
  case res of
    Left err -> error $ show err
    Right x  -> return x


data Bin = W Word16 | LD String | LU String | S String [Word16] | VLit String Word16 | VLabel String String | A Word16 | AL Word16 String

instance Show Bin where
  show (W w) = showHex w
  show (LD s) = "LD " ++ s
  show (LU s) = "LU " ++ s
  show (S s v) = "S " ++ s ++ ": " ++ unwords (map showHex v)
  show (VLit s v) = "VLit " ++ s ++ ": " ++ showHex v
  show (VLabel s l) = "VLabel " ++ s ++ ": " ++ l
  show (A len) = "A[" ++ show len ++ "]"
  show (AL len label) = "AL[" ++ show len ++ "] " ++ label

-- almost raw bytes here, just with markers for label definitions and placeholders for label uses.
buildBinary :: [Asm] -> [Bin]
buildBinary = concatMap assemble

assemble :: Asm -> [Bin]
assemble (LabelDef x) = [LD x]
assemble Blank = []
assemble (Instr op a Nil) = let (d, x) = assembleArg a 
                            in  [W $ shiftL d 10 .|. shiftL (assembleExtOp op) 4] ++ x
assemble (Instr op a b) = let (dA, xA) = assembleArg a
                              (dB, xB) = assembleArg b
                          in  [W $ shiftL dB 10 .|. shiftL dA 4 .|. assembleOp op] ++ xA ++ xB
assemble (LitWord w) = [W w]
assemble (LitLabel label) = [LU label]
assemble (LitASCII s) = map (W . fromIntegral . ord) s
assemble (Symbol s v) = [S s v]
assemble (VarLit s v) = [VLit s v]
assemble (VarLabel s label) = [VLabel s label]
assemble (Array len) = [A len]
assemble (ArrayLabel len label) = [AL len label]


assembleExtOp "JSR" = 0x01
assembleExtOp "DIE" = 0x02
assembleExtOp "INP" = 0x03
assembleExtOp "OUT" = 0x04
assembleExtOp "HEX" = 0x05
assembleExtOp "BRK" = 0x06
assembleExtOp "RND" = 0x07


assembleOp "SET" = 0x01
assembleOp "ADD" = 0x02
assembleOp "SUB" = 0x03
assembleOp "MUL" = 0x04
assembleOp "DIV" = 0x05
assembleOp "MOD" = 0x06
assembleOp "SHL" = 0x07
assembleOp "SHR" = 0x08
assembleOp "AND" = 0x09
assembleOp "BOR" = 0x0a
assembleOp "XOR" = 0x0b
assembleOp "IFE" = 0x0c
assembleOp "IFN" = 0x0d
assembleOp "IFG" = 0x0e
assembleOp "IFB" = 0x0f


-- returns the 6-bit (unshifted) argument description that goes in the opcode word, and a list of Bin values to follow the opcode word.
assembleArg :: Arg -> (Word16, [Bin])
assembleArg (Reg r) = (r, []) -- A
assembleArg (RegAddr r) = (r+0x08, []) -- [A]
assembleArg (LitAndReg a r) = (r+0x10, [W a]) -- [0x1000 + A]
assembleArg (LitAddr a) = (0x1e, [W a])       -- [0x1000]
assembleArg (LabelAddr label) = (0x1e, [LU label]) -- [label]
assembleArg (Lit w) | w < 0x20  = (w + 0x20, []) -- inline literal
                    | otherwise = (0x1f, [W w])  -- next word literal
assembleArg (LabelUse label) = (0x1f, [LU label])
assembleArg Pop  = (0x18, [])
assembleArg Peek = (0x19, [])
assembleArg Push = (0x1a, [])
assembleArg SP   = (0x1b, [])
assembleArg PC   = (0x1c, [])
assembleArg O    = (0x1d, [])



-- returns a map of labels to their values
populateLabels :: [Bin] -> Map String [Word16]
populateLabels = pop' M.empty 0
  where pop' labels addr [] = labels
        pop' labels addr (LD label : bins) = pop' (M.insert label [addr] labels) addr bins
        pop' labels addr (W w : bins) = pop' labels (addr+1) bins
        pop' labels addr (LU label : bins) = pop' labels (addr+1) bins
        pop' labels addr (S label values : bins) = pop' (M.insert label values labels) addr bins
        pop' labels addr (VLit _ _ : bins) = pop' labels addr bins
        pop' labels addr (VLabel _ _ : bins) = pop' labels addr bins
        pop' labels addr (A len : bins) = pop' labels (addr+len) bins
        pop' labels addr (AL len _ : bins) = pop' labels (addr+len) bins



rawCode :: [Bin] -> Map String [Word16] -> [Word16]
rawCode bins labels = concat . snd $ mapAccumL raw M.empty bins
  where raw vars (LD _) = (vars, [])
        raw vars (W w) = (vars, [w])
        raw vars (LU label) = case M.lookup label vars of
                                Just a  -> (vars, [a])
                                Nothing -> case M.lookup label labels of
                                             Just a  -> (vars, a)
                                             Nothing -> error $ "Unknown label/variable: '" ++ label ++ "'"
        raw vars (S _ _) = (vars, [])
        raw vars (VLit s v) = (M.insert s v vars, [])
        raw vars (VLabel s v) = case M.lookup v vars of
                                  Just a -> (M.insert s a vars, [])
                                  Nothing -> case M.lookup v labels of
                                               Just [a] -> (M.insert s a vars, [])
                                               Just _ -> error $ "Label contents too long to fit in variable."
                                               Nothing -> error $ "Unknown label/variable: '" ++ v ++ "'"
        raw vars (AL len label) = case M.lookup label labels of
                                    Just a  -> (vars, a)
                                    Nothing -> error $ "Unknown label: '" ++ label ++ "'"
        raw vars (A len) = (vars, replicate (fromIntegral len) 0)


main :: IO ()
main = do
  [asmFile, outFile] <- getArgs
  asm <- parseFile asmFile
  let bin = buildBinary asm
      labels = populateLabels bin
      code = rawCode bin labels
      (_, bs) = runPutM $ mapM putWord16be code
  --print asm
  --print labels
  h <- openFile "labels.out" WriteMode
  mapM_ (\(k,v) -> hPutStrLn h $ showHex (head v) ++ " " ++ k) (M.assocs labels)
  hFlush h
  B.writeFile outFile bs


