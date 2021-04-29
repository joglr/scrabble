module internal PlayersState
    type PlayersState

    val create : uint32 -> uint32 -> PlayersState
    val next : PlayersState -> PlayersState
    val current : PlayersState -> uint32
    val forfeit : PlayersState -> PlayersState