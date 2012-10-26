module LegalMoves where

import GameData

-- Maybe change definition of Move (or return (Move, sourcepos) / ExtendedMove)

moveSquare _ Nothing _ = []
moveSquare tomove (Just color piece) _
  | tomove == color = movePiece piece
  | otherwise = []
                
movePiece Knight pos@(x, y) =
  let deltas = [(x-2, y-1), (x-2, y+1), (x-1, y-2), (x-1, y+2),
                (x+2, y-1), (x+2, y+1), (x+1, y-2), (x+1, y+2)]
  in
   map (Move Knight pos) deltas
   
-- King
-- rook
-- bishop
-- queen
-- pawn

legalMoves board tomove castles ep =
  filter (extendedLegal tomove castles ep) $ concatMap (moveSquare tomove) board
  
--extendedLegal filters
-- out of range
-- move into check
-- non-allowed ep
-- non-allowed castle (castling in or past check is checked in move, though)

--build space

--define starting position and starting space

--make position counter
--test


