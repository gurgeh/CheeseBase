module GameData 
       where

import Data.Char
import qualified Data.Map as Map


data Piece = Pawn | Knight | Bishop | Rook | Queen | King
instance Show Piece where
    show Pawn = ""
    show Knight = "N"
    show Bishop = "B"
    show Rook = "R"
    show Queen = "Q"
    show King = "K"

pieceList :: [(Char, Piece)]
pieceList = [('N', Knight), ('B', Bishop), ('R', Rook), ('Q', Queen), ('K', King)]

pieceMap :: Map.Map Char Piece
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

showNormalPawn :: Square -> Maybe Piece -> String
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
