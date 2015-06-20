type tcontent = U | D | T | Q

type player = {name:string; symbol:tcontent}

type tsmallgrid = {content:tcontent list; status: tcontent}

type tbigGrid = tsmallgrid list

val drawSmallGrid : tsmallgrid -> int -> int ->unit
val drawBigGrid : tbigGrid -> unit

val winBigGrid: tbigGrid -> tcontent

val modifSmallGrid: tsmallgrid -> int -> tcontent -> bool -> tsmallgrid
val modifBigGrid: tbigGrid -> int -> int -> tcontent -> tbigGrid

val cellIsFull: tsmallgrid -> bool

val getInfoPlayer: tcontent -> player
val getMove: player -> tbigGrid -> int * int

val launchGame : player -> player -> tbigGrid -> tcontent