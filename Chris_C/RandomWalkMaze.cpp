#include "writebitmap.h"
#include <random>
#include <algorithm>
#include <list>

const int MazeWidth = 50;
const int MazeHeight = 50;
const int CellWidth = 10;
const int CellHeight = 10;

void RandomWalkMaze()
{
    Color col;

    Bitmap* pBitmap = CreateBitmap((MazeWidth * CellWidth) + 1, (MazeHeight * CellHeight) + 1);

    // Directions
    enum Direction
    {
        North = 0,
        East = 1,
        South = 2,
        West = 3
    };

    // Cells with walls
    struct Cell
    {
        bool walls[4]{ true, true, true, true };
        bool visited = false;
        int x;
        int y;
        int distance = -1;
    };

    // Initialize the cell coordinates
    Cell cells[MazeWidth][MazeHeight];
    for (int y = 0; y < MazeHeight; y++)
    {
        for (int x = 0; x < MazeHeight; x++)
        {
            cells[x][y].distance = -1;
            cells[x][y].x = x;
            cells[x][y].y = y;
        }
    }

    // Return a given cell
    auto getCell = [&](int x, int y) { return &cells[x][y]; };

    // Get the opposite direction
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

    // Get an adjacent cell
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

        if (x < 0 || x >= MazeWidth || y < 0 || y >= MazeHeight)
        {
            return nullptr;
        }

        return getCell(x, y);
    };

    // Make the hole
    auto makeHole = [&](int x, int y, Direction dir)
    {
        Cell* pCell = getCell(x, y);
        if (pCell)
        {
            pCell->walls[dir] = false;
            auto pAdjacent = getAdjacent(x, y, dir);
            if (pAdjacent)
            {
                pAdjacent->walls[getOpposite(dir)] = false;
            }
        }
    };

    // Draw cells, with color
    auto draw = [&](float maxDistance)
    {
        col.blue = 0;
        col.green = 0;
        col.red = 0;
        ClearBitmap(pBitmap, col);

        float distanceColorStep = 255.0f / float(maxDistance);
        for (int x = 0; x < MazeWidth; x++)
        {
            for (int y = 0; y < MazeHeight; y++)
            {
                auto cell = getCell(x, y);

                if (cell->distance == -1)
                {
                    col.red = 0;
                    col.green = 0;
                }
                else
                {
                    col.red = uint8_t(255.0f - (distanceColorStep * cell->distance));
                    col.red = std::max((uint8_t)0, col.red);
                    col.green = 255 - col.red;
                }        
                
                col.blue = 0;

                auto xPos = x * CellWidth;
                auto yPos = y * CellWidth;

                DrawBlock(pBitmap, xPos + 2, yPos + 2, xPos + CellWidth - 1, yPos + CellHeight - 1, col);

                col.red = 255;
                col.green = 255;

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
    };

    // Random Walk - lets start from a random point
    auto currentX = (rand() % MazeWidth);
    auto currentY = (rand() % MazeHeight);
 
    auto NumToVisit = MazeWidth * MazeHeight;
    while (NumToVisit > 0)
    {
        // Walk in a random direction
        auto dir = Direction(rand() % 4);
        auto targetCell = getAdjacent(currentX, currentY, dir);
        if (targetCell)
        {
            if (targetCell->visited == false)
            {
                // We haven't visited this one, so make the whole back in the direction we walkd
                makeHole(currentX, currentY, dir);
                NumToVisit--;
                targetCell->visited = true;
            }
            currentX = targetCell->x;
            currentY = targetCell->y;
        }
    }

    // Draw the random walk maze
    draw(0.0f);
    WriteBitmap(pBitmap, "RandomWalk_Maze.bmp");

    // Dijkstra 
    std::list<Cell*> considerCells;
    considerCells.push_back(&cells[0][0]);

    int maxDistance = -1;
    while (!considerCells.empty())
    {
        auto* thisEntry = considerCells.back();
        considerCells.pop_back();
        if (thisEntry->distance == -1)
        {
            thisEntry->distance = 0;
        }

        auto* cell = getCell(thisEntry->x, thisEntry->y);
        for (int i = 0; i < 4; i++)
        {
            if (cell->walls[i] == false)
            {
                Cell* entry;
                if (entry = getAdjacent(thisEntry->x, thisEntry->y, (Direction)i))
                {
                    if (entry->distance == -1)
                    {
                        entry->distance = thisEntry->distance + 1;
                        considerCells.push_back(entry);
                        maxDistance = std::max(entry->distance, maxDistance);
                    }
                }
            }
        }
    }

    // Redraw, but this time show the texture
    draw(float(maxDistance));
    WriteBitmap(pBitmap, "RandomWalk_TextureTopToBottom.bmp");

    // Find the shortest path by walking from the end back to the beginning,
    // while travelling along the minimum distance route
    Cell* cell = getCell(MazeWidth - 1, MazeHeight - 1);

    while (cell->distance != 0)
    {
        Cell* nextCell = nullptr;

        int minDist = cell->distance;

        for (int i = 0; i < 4; i++)
        {
            if (!cell->walls[i])
            {
                Cell* testCell = getAdjacent(cell->x, cell->y, (Direction)i);

                if (testCell &&
                    testCell->distance != -1 &&
                    testCell->distance < minDist)
                {
                    minDist = testCell->distance;
                    nextCell = testCell;
                }
            }
        }

        assert(nextCell);

        cell->distance = -1;
        cell = nextCell;
    }

    cell->distance = -1;

    // Redraw, but this time show the shortest path
    draw(float(maxDistance));
    WriteBitmap(pBitmap, "RandomWalk_ShortPath.bmp");

    system("start RandomWalk_ShortPath.bmp");
    system("start RandomWalk_TextureTopToBottom.bmp");
    system("start RandomWalk_Maze.bmp");

    DestroyBitmap(pBitmap);
}
