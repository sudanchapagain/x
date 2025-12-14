local screenWidth, screenHeight = 600, 600
local gridSize = 5
local cellSize = screenWidth / gridSize

local board = {}
local tigers = {}
local goats = {}

local selectedPiece = nil
local currentPlayer = "goat"
local goatCount = 20
local placedGoats = 0

function love.load()
    love.window.setMode(screenWidth, screenHeight)
    love.window.setTitle("Baghchal")

    for x = 1, gridSize do
        board[x] = {}
        for y = 1, gridSize do
            board[x][y] = nil
        end
    end

    local tigerPositions = {
        {1, 1}, {1, 5}, {5, 1}, {5, 5}
    }

end

function love.draw()
end
