module LegalMoves where

import GameData

{- Todo:
    pawn
      move
      take
      ep
      promo
    castling
    filter take own pieces
    king and knight out of range
--
    filter move into check

    filter castle past check

    define start position
    test manually

    define game space / something traversable?
    make position counter and test against standard positions

    unittest position counting
    connect to PGN parser
    unittest that all moves in a PGN db are correct and unambiguous
-}

getSquare (Board board _ _ _ ) x y =
  board !! y !! x
  

moveSquare _ Nothing _ = []
moveSquare board tomove (Just color piece) _
  | tomove == color = movePiece board piece
  | otherwise = []
                
extend board (x, y) dx dy =
  let subextend x y dx dy 
        | x < 0 || y < 0 = []
        | x >= 8 || y >= 8 = []
        | getSquare board x y == Nothing = [(x, y)]
        | otherwise = (x, y) : subextend x + dx y + dy dx dy
  in
   subextend x + dx y + dy dx dy ++ subextend x - dx y - dy (-dx) (-dy)

movePiece _ Knight pos@(x, y) =
  let 
    deltas = [-2, -1, 1, 2]
    moves = [(x + a, y + b) | a <- deltas, b <- deltas, abs a /= abs b]
  in
   map (Move Knight pos) moves

--check if castling ok here
movePiece board King pos@(x, y) =
  let deltas = [(x + a, y + b) | a <- [-1..1], b <- [-1..1], not (a == 0 && b == 0)]
  in
   CastleKS : CastleQS : map (Move King pos) deltas


movePiece board Rook pos@(x, y) =
  map (Move Rook pos) $ extend board pos 1 0 ++ extend board pos 0 1
  
movePiece board Bishop pos@(x, y) =
  map (Move Bishop pos) $ extend board pos 1 1 ++ extend board pos 1 -1

movePiece board Queen pos =
  movePiece board Bishop pos ++ movePiece board Rook pos

-- pawn

legalMoves board tomove castles ep =
  filter (extendedLegal tomove castles ep) $ concatMap (moveSquare board tomove) board
  



