-- Try infinite generator with TicTacToe first

type GameTree = Board [(Move, GameTree)]

generateGameTree board =