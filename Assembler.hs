{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, BangPatterns, NoMonomorphismRestriction #-}

-- An emulator for DCPU-16, the 16-bit virtual machine designed by Notch for 0x10c.
-- This is based on the specification version 1.7.
-- Supports DAT syntax same as das.
-- It also includes the nonstandard but handy HCF "halt and catch fire" instruction (compiled as extended opcode 7).

import Data.Word
import Data.Char
import Data.Bits
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Maybe
import Data.List

import System.IO
import System.Environment (getArgs)
import System.Exit
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

data Arg = Reg Word16 | RegAddr Word16 | LitAddr Word16 | LitAndReg Word16 Word16 | Lit Word16 | LabelUse String | LabelAddr String | Peek | Push | Pop | SP | PC | EX | Nil | Expr Arg String Arg
  deriving (Show)

data Asm = LabelDef String
         | Instr String Arg Arg
         | Blank
         | LitWord Word16
         | LitString String
         | LitLabel String
  deriving (Show)


showHex :: Word16 -> String
showHex x = printf "%04x" x

-- parses a label: begins with a letter or underscore, continues with a letter, number or underscore.
pl = (:) <$> (oneOf "_-" <|> letter) <*> many (oneOf "_-." <|> alphaNum)

parser = fmap concat $ many $ do
  x <- try pComment <|> try pDAT <|> pLine
  spaces
  return x

pWS = skipMany (oneOf " \t\r")

pComment = pWS *> char ';' *> manyTill anyChar (try newline) *> pure [Blank]


pDAT = do
    spaces
    string "DAT "
    xs <- sepBy (try pASCII <|> pNumber <|> pLitLabel) (pWS >> char ',' >> pWS >> return ())
    spaces
    return xs
  where pASCII = do
          char '"'
          x <- many (noneOf "\"")
          char '"'
          return $ LitString x
        pNumber = do
          Lit word <- pLit
          return $ LitWord word
        pLitLabel = LitLabel <$> pl

pLine = try pLabel <|> pInstr

pLabel = do
  pWS
  char ':'
  l <- pl
  return [LabelDef l]

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
  return [Instr op a b]

pExtInstr = do
  pWS
  op <- pExtOpcode
  space
  pWS
  a <- pArg
  optional (try pComment)
  return [Instr op a Nil]


pOpcode =  try (string "SET")
       <|> try (string "ADD")
       <|> try (string "SUB")
       <|> try (string "MUL")
       <|> try (string "MLI")
       <|> try (string "DIV")
       <|> try (string "DVI")
       <|> try (string "MOD")
       <|> try (string "MDI")
       <|> try (string "SHL")
       <|> try (string "SHR")
       <|> try (string "ASR")
       <|> try (string "AND")
       <|> try (string "BOR")
       <|> try (string "XOR")
       <|> try (string "IFE")
       <|> try (string "IFN")
       <|> try (string "IFG")
       <|> try (string "IFL")
       <|> try (string "IFC")
       <|> try (string "IFA")
       <|> try (string "IFU")
       <|> try (string "IFB")
       <|> try (string "ADX")
       <|> try (string "SBX")
       <|> try (string "STI")
       <|> string "STD"

pExtOpcode =  try (string "JSR")
          <|> try (string "INT")
          <|> try (string "IAG")
          <|> try (string "IAS")
          <|> try (string "RFI")
          <|> try (string "IAQ")
          <|> try (string "HWN")
          <|> try (string "HWQ")
          <|> try (string "HWI")
          <|> string "HCF"



-- several different kinds of arguments
pArg = try pReg <|> try pRegAddr <|> try pLitAndReg <|> try pLitAddr <|> try pPeek <|> try pPush <|> try pPop <|> try pSP <|> try pPC <|> try pEX <|> try pLit <|> pLabelUse

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
pEX   = string "EX"   >> return EX

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


data Bin = W Word16 | LD String | LU String

instance Show Bin where
    show (W w) = showHex w
    show (LD s) = "LD " ++ s
    show (LU s) = "LU " ++ s

-- almost raw bytes here, just with markers for label definitions and placeholders for label uses.
buildBinary :: [Asm] -> [Bin]
buildBinary = concatMap assemble

assemble :: Asm -> [Bin]
assemble (LabelDef x) = [LD x]
assemble Blank = []
assemble (Instr op a Nil) = let (d, x) = assembleArg a
                            in  [W $ shiftL d 10 .|. shiftL (assembleExtOp op) 5] ++ x
assemble (Instr op a b) = let (dA, xA) = assembleArg a
                              (dB, xB) = assembleArg b
                          in  [W $ shiftL dB 10 .|. shiftL dA 5 .|. assembleOp op] ++ xB ++ xA
assemble (LitWord w) = [W w]
assemble (LitLabel label) = [LU label]
assemble (LitString s) = map (W . fromIntegral . ord) s


assembleExtOp "JSR" = 0x01
assembleExtOp "HCF" = 0x07
assembleExtOp "INT" = 0x08
assembleExtOp "IAG" = 0x09
assembleExtOp "IAS" = 0x0a
assembleExtOp "RFI" = 0x0b
assembleExtOp "IAQ" = 0x0c
assembleExtOp "HWN" = 0x10
assembleExtOp "HWQ" = 0x11
assembleExtOp "HWI" = 0x12


assembleOp "SET" = 0x01
assembleOp "ADD" = 0x02
assembleOp "SUB" = 0x03
assembleOp "MUL" = 0x04
assembleOp "MLI" = 0x05
assembleOp "DIV" = 0x06
assembleOp "DVI" = 0x07
assembleOp "MOD" = 0x08
assembleOp "MDI" = 0x09
assembleOp "AND" = 0x0a
assembleOp "BOR" = 0x0b
assembleOp "XOR" = 0x0c
assembleOp "SHR" = 0x0d
assembleOp "ASR" = 0x0e
assembleOp "SHL" = 0x0f
assembleOp "IFB" = 0x10
assembleOp "IFC" = 0x11
assembleOp "IFE" = 0x12
assembleOp "IFN" = 0x13
assembleOp "IFG" = 0x14
assembleOp "IFA" = 0x15
assembleOp "IFL" = 0x16
assembleOp "IFU" = 0x17
assembleOp "ADX" = 0x1a
assembleOp "SBX" = 0x1b
assembleOp "STI" = 0x1e
assembleOp "STD" = 0x1f



-- returns the 6-bit (unshifted) argument description that goes in the opcode word, and a list of Bin values to follow the opcode word.
assembleArg :: Arg -> (Word16, [Bin])
assembleArg (Reg r) = (r, []) -- A
assembleArg (RegAddr r) = (r+0x08, []) -- [A]
assembleArg (LitAndReg a r) = (r+0x10, [W a]) -- [0x1000 + A]
assembleArg (LitAddr a) = (0x1e, [W a])       -- [0x1000]
assembleArg (LabelAddr label) = (0x1e, [LU label]) -- [label]
assembleArg (Lit w) | w <  0x1f   = (w + 0x21, []) -- inline literal
                    | w == 0xffff = (0x20, [])   -- special case for -1
                    | otherwise   = (0x1f, [W w])  -- next word literal
assembleArg (LabelUse label) = (0x1f, [LU label])
assembleArg Pop  = (0x18, [])
assembleArg Peek = (0x19, [])
assembleArg Push = (0x1a, [])
assembleArg SP   = (0x1b, [])
assembleArg PC   = (0x1c, [])
assembleArg EX   = (0x1d, [])



-- Given the binary and the base for relocation, returns a map of labels to their values
populateLabels :: [Bin] -> Word16 -> Map String [Word16]
populateLabels = pop' M.empty
  where pop' labels [] _ = labels
        pop' labels (LD label : bins) addr = pop' (M.insert label [addr] labels) bins addr
        pop' labels (W w : bins) addr = pop' labels bins (addr+1)
        pop' labels (LU label : bins) addr = pop' labels bins (addr+1)

-- Given the binary, returns a list of all the locations where a label is stored, relative to the beginning of the code.
labelUses :: [Bin] -> [Word16]
labelUses bin = pop' [] bin 0
  where pop' uses [] _ = uses
        pop' uses (LD _ : bins) addr = pop' uses bins addr
        pop' uses (W _ : bins) addr = pop' uses bins (addr+1)
        pop' uses (LU label : bins) addr = pop' (addr : uses) bins (addr+1)


rawCode :: [Bin] -> Map String [Word16] -> [Word16]
rawCode bins labels = concatMap raw bins
  where raw (LD _) = []
        raw (W w)  = [w]
        raw (LU label) = case M.lookup label labels of
                             Just a  -> a
                             Nothing -> error $ "Unknown label: '" ++ label ++ "'"


main :: IO ()
main = do
  args <- getArgs
  (relocate, asmFile, outFile) <- case args of
      ["-r", asmFile, outFile] -> return (True, asmFile, outFile)
      [a, asmFile, outFile]    -> hPutStrLn stderr ("Unknown argument: " ++ a) >> exitWith (ExitFailure 1)
      [asmFile, outFile]       -> return (False, asmFile, outFile)
  asm <- parseFile asmFile
  let bin = buildBinary asm
      relocs = labelUses bin
      relocCount = genericLength relocs + 5
  print relocCount
  let labels = populateLabels bin (if relocate then relocCount else 0)
      code = rawCode bin labels
      relocCode = if relocate
            then [0x7f81, relocCount, 0x5254, 0x0001, relocCount-2] ++ map (+relocCount) relocs
            else []
      (_, bs) = runPutM $ mapM putWord16be (relocCode ++ code)
  B.writeFile outFile bs


