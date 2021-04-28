module internal BoardState
    type BoardState

    val empty : BoardState
    val isEmpty : BoardState -> bool
    val addTile : BoardState -> ((int * int) * uint32) -> BoardState
    val addTiles : BoardState -> ((int * int) * uint32) list -> BoardState
    val getTile: BoardState -> (int * int) -> uint32 option