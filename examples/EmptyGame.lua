--[[
local card = {
	suit = "SUIT",
	rank = "RANK",
}

local player = {
	symbol = "SYMBOL",
	name = "NAME",
	moves = {},
	hand = {},
	playedCards = {},
}

local moves = {
  [1] = { ["MOVE" ] = MOVE.PLAY }
}

GAME = {
  deck = { { DECK = "deck", CARDS = {} } },
  ...
}
]]
--

MOVE = {
	PLAY = 1,
	DRAW = 2,
	PASS = 3,
}

GAME_STATE = {
	START = 1,
	POST_START = 2,
	PRE_TURN = 3,
	POST_TURN = 4,
	PRE_ROUND = 5,
	POST_ROUND = 6,
	POST_WIN = 7,
	POST_END = 8,
}

GAME = {
	-- List of all the players
	-- Is empty at the initialization of the game
	players = nil,
	-- What moves a player has
	moves = {},
	-- How many cards a player start with
	hand = 0,
	-- List of all possible cards
	cards = {},
	-- List of decks a player can draw/play/discard on
	deck = {},
	-- Current turn pointer, ie. what player is currently playing
	-- Using 1-indexing
	turnPointer = 0,
	actions = {
		[GAME_STATE.START] = {},
		[GAME_STATE.POST_START] = {},
		[GAME_STATE.PRE_TURN] = {},
		[GAME_STATE.POST_TURN] = {},
		[GAME_STATE.PRE_ROUND] = {},
		[GAME_STATE.POST_ROUND] = {},
		[GAME_STATE.POST_WIN] = {},
		[GAME_STATE.POST_END] = {},
	},
}

-- Initializes the game
function Main(game)
	return game
end
