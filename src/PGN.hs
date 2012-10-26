--Unfortunately I have no idea what these pragmas do.
{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction,
    FlexibleInstances, FlexibleContexts, RankNTypes #-}

module PGN (pGame, pGames, pTags, pMoves) where

import Data.Char
import qualified Data.Map as Map

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances

import GameData

--Todo

-- Hunit against real data
-- QuickCheck?
-- performance on e.g Megabase

--Future
-- store comments
-- store variations
-- Cabal package
-- Hackage


pInverse :: (Char -> Bool) -> Parser Char
pInverse f = pSatisfy (not . f) (Insertion "inverse error" 'y' 5)

pIdent :: Parser String
pIdent = lexeme $ pList $ pInverse isSpace

pEscQuoted :: Parser String
pEscQuoted = lexeme $ pSym '"' *> pList pOKChar <* pSym '"'
    where
        pEscape = pSym '\\' *> (pSym '"' <|> pSym '\\')
        pNonQuote = pInverse ('"' ==)
        pOKChar = pEscape <<|> pNonQuote

pTagKeyVal :: Parser Tag
pTagKeyVal = Tag <$> pIdent <*> pEscQuoted

pTag :: Parser Tag
pTag = pLBracket *> pTagKeyVal <* pRBracket

pTags :: Parser [Tag]
pTags = lexeme $ pList1 pTag

charToPiece :: Char -> Piece
charToPiece = (Map.!) pieceMap

pPiece :: Parser Piece
pPiece = charToPiece <$> pAnySym (Map.keys pieceMap)

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

isParen :: Char -> Bool
isParen '(' = True
isParen ')' = True
isParen _ = False

pVMoves :: Parser [Char]
pVMoves = pList (pInverse isParen)

pVariation :: Parser Int
pVariation =  (+1) <$> (pSym '(' *> ((pVMoves *> pVariation <* pVMoves) <|> (\_ -> 0) <$> pVMoves) <* pSym ')')

pMove :: Parser Move
pMove = lexeme ((pPawnMove <<|> pStandardMove <<|> pCastle) <*
    pMaybe pCheckOrMate) <* pMaybe pAnnotation <* pMaybe pComment <* pMaybe pVariation

pMovePair :: Parser (Move, Maybe Move)
pMovePair = (,) <$> pMove <*> pMaybe pMove

pNumberedMove :: Parser (Move, Maybe Move)
pNumberedMove = (pInteger::Parser Integer) *> pDot *> pMovePair <* pMaybe pResult

pMoves :: Parser [(Move, Maybe Move)]
pMoves = lexeme $ pList1 pNumberedMove

pResult :: Parser String
pResult = lexeme $ pEnumStrs ["1/2-1/2", "1-0", "0-1", "*"]

flattenMove :: (Move, Maybe Move) -> [Move] -> [Move]
flattenMove (g1, Nothing) gs = g1 : gs
flattenMove (g1, Just g2) gs = g1 : g2 : gs

flattenMoves :: [(Move, Maybe Move)] -> [Move]
flattenMoves = foldr flattenMove []

makeGame :: [Tag] -> [(Move, Maybe Move)] -> Game
makeGame tags moves = Game tags $ flattenMoves moves

pGame :: Parser Game
pGame = makeGame <$> pTags <*> pMoves

pGames :: Parser [Game]
pGames = pList pGame