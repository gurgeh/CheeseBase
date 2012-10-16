--Unfortunately I have no idea what these pragmas do.
{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction,
    FlexibleInstances, FlexibleContexts, RankNTypes #-}

module PGN where

import Data.Time
import Data.Char
import qualified Data.Map as Map
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived
import Text.ParserCombinators.UU.Core
import Control.Monad

--Todo
-- move data types to separate file
-- check in
-- clean up. Add signatures and module exports. Remove superfluous imports and pragmas

-- Hunit against real data
-- QuickCheck?
-- performance on e.g Megabase

--Future
-- store comments
-- store variations
-- Cabal package
-- Hackage


data Piece = Pawn | Knight | Bishop | Rook | Queen | King
instance Show Piece where
    show Pawn = ""
    show Knight = "N"
    show Bishop = "B"
    show Rook = "R"
    show Queen = "Q"
    show King = "K"

pieceList = [('N', Knight), ('B', Bishop), ('R', Rook), ('Q', Queen), ('K', King)]
pieceMap = Map.fromList pieceList


data File = File Int
instance Show File where
    show (File x) = [chr $ fromEnum 'a' + x]

data Rank = Rank Int
instance Show Rank where
    show (Rank i) = show (i + 1)

data Square = Square File Rank
instance Show Square where
    show (Square f r) = show f ++ show r

data HelpSquare = HelpSquare (Maybe File) (Maybe Rank)
instance Show HelpSquare where
    show (HelpSquare Nothing Nothing) = ""
    show (HelpSquare (Just f) Nothing) = show f
    show (HelpSquare Nothing (Just r)) = show r
    show (HelpSquare (Just f) (Just r)) = show f ++ show r

data Move = Move Piece HelpSquare Square |
            PawnMove (Maybe File) Square (Maybe Piece) |
            CastleKS |
            CastleQS

showNormalPawn s Nothing = show s
showNormalPawn s (Just p) = show s ++ ('=' : show p)

instance Show Move where
    show CastleKS = "O-O"
    show CastleQS = "O-O-O"
    show (PawnMove Nothing s mp) = showNormalPawn s mp
    show (PawnMove (Just f) s mp) = show f ++ showNormalPawn s mp
    show (Move p hs s) = show p ++ show hs ++ show s

data Tag = Tag String String deriving Show
data Result = WhiteWin | BlackWin | Draw deriving Show

data Game = Game [Tag] [Move] deriving Show

pInverse f = pSatisfy (not . f) (Insertion "inverse error" 'y' 5)

pIdent :: Parser String
pIdent = lexeme $ pList $ pInverse isSpace

pEscQuoted = lexeme $ pSym '"' *> pList pOKChar <* pSym '"'
    where
        pEscape = pSym '\\' *> (pSym '"' <|> pSym '\\')
        pNonQuote = pInverse ('"' ==)
        pOKChar = pEscape <<|> pNonQuote


pTagKeyVal = Tag <$> pIdent <*> pEscQuoted
pTag = pLBracket *> pTagKeyVal <* pRBracket
pTags = lexeme $ pList1 pTag

charToPiece :: Char -> Piece
charToPiece = (Map.!) pieceMap

pPiece :: Parser Piece
pPiece = charToPiece <$> pAnySym $ Map.keys pieceMap

pFile :: Parser File
pFile = (\x -> File (fromEnum x - fromEnum 'a')) <$> pRange ('a', 'h')

pRank :: Parser Rank
pRank = (\x -> Rank (fromEnum x - fromEnum '1')) <$> pRange ('1', '8')

pSquare :: Parser Square
pSquare = Square <$> pFile <*> pRank

pPawnMove :: Parser Move
pPawnMove = PawnMove <$> pMaybe (pFile <* pSym 'x') <*> pSquare <*> pMaybe (pSym '=' *> pPiece)

pHelpSquare :: Parser HelpSquare
pHelpSquare = HelpSquare <$> pMaybe pFile <*> pMaybe pRank

pStandardMove :: Parser Move
pStandardMove = Move <$> pPiece <*> pHelpSquare <*> (pMaybe (pSym 'x') *> pSquare)

longOrShort :: Maybe String -> Move
longOrShort Nothing = CastleKS
longOrShort (Just _) = CastleQS

pCastle :: Parser Move
pCastle = longOrShort <$> (pSymbol "O-O" *> pMaybe (pSymbol "-O"))

pCheckOrMate :: Parser Char
pCheckOrMate = pSym '+' <|> pSym '#'

pAnnotation :: Parser Int
pAnnotation = pSym '$' *> pInteger

pComment :: Parser String
pComment = lexeme $ pSym '{' *> pList (pInverse ('}' ==)) <* pSym '}'

isParen '(' = True
isParen ')' = True
isParen _ = False

pVMoves = pList (pInverse isParen)

pVariation :: Parser Int
pVariation =  (+1) <$> (pSym '(' *> ((pVMoves *> pVariation <* pVMoves) <|> (\_ -> 0) <$> pVMoves) <* pSym ')')

pMove :: Parser Move
pMove = lexeme ((pPawnMove <<|> pStandardMove <<|> pCastle) <*
    pMaybe pCheckOrMate) <* pMaybe pAnnotation <* pMaybe pComment <* pMaybe pVariation

pMovePair :: Parser (Move, Maybe Move)
pMovePair = (,) <$> pMove <*> pMaybe pMove

pNumberedMove :: Parser (Move, Maybe Move)
pNumberedMove = pInteger *> pDot *> pMovePair <* pMaybe pResult

--pMoves :: Parser [Move]
pMoves = lexeme $ pList1 pNumberedMove

pResult :: Parser String
pResult = lexeme $ pEnumStrs ["1/2-1/2", "1-0", "0-1", "*"]

flattenMove (g1, Nothing) gs = g1 : gs
flattenMove (g1, Just g2) gs = g1 : g2 : gs
flattenMoves = foldr flattenMove []

makeGame tags moves = Game tags $ flattenMoves moves

pGame = makeGame <$> pTags <*> pMoves
pGames = pList pGame