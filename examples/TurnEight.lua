local game = {
	plrs = {},
	movs = {},
	hand = 0,
	crds = {},
	deck = {},
	pile = {},
	disc = {},
}

local card = {
	suit = "SUIT",
	rank = "RANK",
}

local player = {
	symbol = "SYMBOL",
	name = "NAME",
	moves = {},
	hand = {},
}

MOVE = {
	PLAY = 0,
	DRAW = 1,
	PASS = 2,
}

local moves = {
	-- Move, does end turn
	{ MOVE.PLAY, true },
	{ MOVE.DRAW, 2, true },
	{ MOVE.PASS },
}

function Main(game)
	game.crds = CreateCards()
	game.movs = {
		{ MOVE.PLAY, true },
		{ MOVE.DRAW, 1, false },
		{ MOVE.DRAW, 1, false },
		{ MOVE.DRAW, 1, false },
	}
	game.hand = 5

	return game
end

function WinCon(game)
	for _, p in pairs(game.plrs) do
		if next(p.hand) == nil then
			return p
		end
	end
end

function EndCon(game)
	for _, p in pairs(game.plrs) do
		if next(p.hand) == nil then
			return true
		end
	end
	return false
end

function CreateCards()
	local suits = { "Hearts", "Spades", "Clubs", "Clover" }
	local ranks =
		{ "Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Knight", "Queen", "King" }
	local cards = {}
	local i = 1
	for _, s in pairs(suits) do
		for _, r in pairs(ranks) do
			cards[i] = { suit = s, rank = r }
		end
	end

	return cards
end
