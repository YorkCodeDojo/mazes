#include "writebitmap.h"
#include <random>
#include <list>
#include <algorithm>

const int MazeWidth = 50;
const int MazeHeight = 50;
const int CellWidth = 10;
const int CellHeight = 10;

void BinaryTreeMaze()
{
    Color col;

    Bitmap* pBitmap = CreateBitmap((MazeWidth * CellWidth) + 1, (MazeHeight * CellHeight) + 1);

    col.blue = 0;
    col.green = 0;
    col.red = 0;
    ClearBitmap(pBitmap, col);

    // 4 Exit directions
    enum Direction
    {
        North = 0,
        East = 1,
        South = 2,
        West = 3
    };

    // A cell with 4 walls, coordinates, and a distance value for the path calculation
    struct Cell
    {
        bool walls[4]{ true, true, true, true };
        int x;
        int y;
        int Distance = -1;
    };

    // The maze cells in an array
    Cell cells[MazeWidth][MazeHeight];

    // Return a cell.  Cheat and init the coords on first access.
    auto getCell = [&](int x, int y) 
    {
        cells[x][y].x = x;
        cells[x][y].y = y;
        return &cells[x][y]; 
    };

    // Get the opposite direction to a given direction
    auto getOpposite = [](Direction dir)
    {
        switch (dir)
        {
        case Direction::North:
            return Direction::South;
        case Direction::East:
            return Direction::West;
        case Direction::South:
            return Direction::North;
        case Direction::West:
            return Direction::East;
        default:
            return Direction::East;
            break;
        }
    };

    // Get a cell in a given direction from a cell
    auto getAdjacent = [&](int x, int y, Direction dir)
        -> Cell*
    {
        switch (dir)
        {
        case Direction::North:
            y--;
            break;
        case Direction::East:
            x++;
            break;
        case Direction::South:
            y++;
            break;
        case Direction::West:
            x--;
            break;
        }

        // Out of bounds
        if (x < 0 || x >= MazeWidth || y < 0 || y >= MazeHeight)
        {
            return nullptr;
        }

        return getCell(x, y);
    };

    // Make a door
    auto makeHole = [&](int x, int y, Direction dir)
    {
        Cell* pCell = getCell(x, y);
        if (pCell)
        {
            pCell->walls[dir] = false;
            auto pAdjacent = getAdjacent(x, y, dir);
            if (pAdjacent)
            {
                // Assign the adjacent cell wall too
                pAdjacent->walls[getOpposite(dir)] = false;
            }
        }
    };

    // Binary walker.  Make holes in east or north wall
    for (auto x = 0; x < MazeWidth; x++)
    {
        for (auto y = 0; y < MazeHeight; y++)
        {
            auto rnd = rand() / float(RAND_MAX);

            auto dir = rnd > .5f ? Direction::North : Direction::East;
            if (y == 0)
            {
                dir = Direction::East;
                if (x == MazeWidth - 1)
                {
                    continue;
                }
            }
            if (x == MazeWidth - 1)
            {
                dir = Direction::North;
            }
            makeHole(x, y, dir);
        }
    }
    
    // Djikstra distance generation
    std::list<Cell*> considerCells;
    considerCells.push_back(&cells[MazeWidth / 2][MazeHeight / 2]);

    int maxDistance = -1;
    while (!considerCells.empty())
    {
        // Remove dealt with cells as we go
        auto* thisEntry = considerCells.back();
        considerCells.pop_back();
        if (thisEntry->Distance == -1)
        {
            thisEntry->Distance = 0;
        }

        auto* cell = getCell(thisEntry->x, thisEntry->y);
        for (int i = 0; i < 4; i++)
        {
            // Find cells we can walk to, add them to our stack, and assign distance + 1
            if (cell->walls[i] == false)
            {
                auto adjacent = getAdjacent(thisEntry->x, thisEntry->y, (Direction)i);
                if (adjacent)
                {
                    if (adjacent->Distance == -1)
                    {
                        adjacent->Distance = thisEntry->Distance + 1;
                        considerCells.push_back(adjacent);
                        maxDistance = std::max(adjacent->Distance, maxDistance);
                    }
                }
            }
        }
    }

    col.green = 255;
    col.blue = 0;
    col.red = 0;

    float distanceColorStep = 255.0f / float(maxDistance);

    // Draw the cells, and color the walls based on distance
    for (int x = 0; x < MazeWidth; x++)
    {
        for (int y = 0; y < MazeHeight; y++)
        {
            float green = 255.0f - (distanceColorStep * cells[x][y].Distance);
            col.green = uint8_t(std::max(0.0f, green));
            
            auto xPos = x * CellWidth;
            auto yPos = y * CellWidth;
            if (getCell(x, y)->walls[Direction::North])
            {
                DrawLine(pBitmap, xPos, yPos, xPos + CellWidth, yPos, col);
            }
            if (getCell(x, y)->walls[Direction::East])
            {
                DrawLine(pBitmap, xPos + CellWidth, yPos, xPos + CellWidth, yPos + CellHeight, col);
            }
            if (getCell(x, y)->walls[Direction::West])
            {
                DrawLine(pBitmap, xPos, yPos, xPos, yPos + CellHeight, col);
            }
            if (getCell(x, y)->walls[Direction::South])
            {
                DrawLine(pBitmap, xPos, yPos + CellHeight, xPos + CellWidth, yPos + CellHeight, col);
            }
        }
    }

    WriteBitmap(pBitmap, "BinaryTree_Maze.bmp");

    system("start BinaryTree_Maze.bmp");

    DestroyBitmap(pBitmap);
}
