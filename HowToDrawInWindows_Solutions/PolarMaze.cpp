
#include <windows.h>
#include <objidl.h>
#include <gdiplus.h>
using namespace Gdiplus;

#include <random>
#include <vector>
#include <algorithm>
#include <cmath>

#undef min
#undef max

// Note this currently gets called on every paint!
void PolarMaze(Graphics& graphics, RECT& rc)
{
    int minRadius = std::min((rc.right - rc.left) / 2, (rc.bottom - rc.top) / 2);

    const double MazeBorder = 2.0f;
    const double Radius = minRadius - (MazeBorder);

    const int Rings = 15;
    const double CellSectorLength = Radius / Rings;
    const double CellSectorAngle = 360.0 / 6.0;
    const int CellSectors = (int)(360.0 / CellSectorAngle);

    double centerBitmapX = Radius + MazeBorder;
    double centerBitmapY = Radius + MazeBorder;

    // Clear window background
    SolidBrush brush(Color::WhiteSmoke);
    graphics.Clear(Color::Black);

    struct Cell;

    // A link between cells, with door/hole
    struct Link
    {
        Cell* cell;
        bool hole;
    };

    // Possible directions leaving a cell
    enum Direction
    {
        Turnwise,
        Widdershins,
        RimwardA,
        RimwardB,
        Hubward,
        DirCount
    };

    struct Cell
    {
        Link link[DirCount];        // Links to other cells
        bool visited = false;
    };

    // Rings and segments
    std::vector<std::vector<Cell>> cells;

    // Modulus of number, but wrap negative too
    auto mod = [](int x, int m) { return (x%m + m) % m; };

    // Returns a cell, and allows segments to wrap
    auto getCell = [&](int ring, int segment)
        -> Cell*
    {
        // Outside the maze
        if (ring < 0 || ring >= Rings)
        {
            return nullptr;
        }
        int numSegments = cells[ring].size();

        segment = mod(segment, numSegments);
        
        return &cells[ring][segment];
    };

    // Get the cell adjacent to the given in a given direction
    auto getAdjacent = [&](Cell* cell, Direction dir)
    {
        return cell->link[dir].cell;
    };

    // Find the door from a given cell back to the target
    auto findLink = [&](Cell* pCell, Cell* pTarget)
        -> Link*
    {
        for (auto& link : pCell->link)
        {
            if (link.cell && link.cell == pTarget)
            {
                return &link;
            }
        }
        return nullptr;
    };

    // Make a hole in a cell
    auto makeHole = [&](Cell* cell, Direction dir)
    {
        cell->link[dir].hole = true;
        if (cell->link[dir].cell != nullptr)
        {
            auto link = findLink(cell->link[dir].cell, cell);
            if (link)
            {
                link->hole = true;
            }
        }
    };

    // Construct the cell data - rings of cells.
    int totalCells = 0;
    cells.resize(Rings);
    for (int i = 0; i < Rings; i++)
    {
        // Figure out the number of cell sectors we want (at least as
        // big as the previous ring)
        auto cellSectors = CellSectors;
        if (i > 1)
        {
            cellSectors = cells[i - 1].size();
        }

        // Figure out the ratio of segment length to ring wall
        // Radius of the Sector
        double cellWidth = 2.0 * std::_Pi * (CellSectorLength * (i + 1));
        cellWidth /= cellSectors;
        int ratio = int(cellWidth / CellSectorLength);
        ratio = std::max(1, ratio);

        cells[i].resize(cellSectors * ratio);
        totalCells += cells[i].size();
    }

    for (int i = 0; i < Rings; i++)
    {
        for (int sector = 0; sector < int(cells[i].size()); sector++)
        {
            // Figure out the ratio between this ring and the previous/next ring segment counts
            auto cell = getCell(i, sector);
            int downRatio = 1; 
            if (i > 0)
            {
                downRatio = cells[i].size() / cells[i - 1].size();
            }
            int upRatio = 1;
            if (i < (Rings - 1))
            {
                upRatio = cells[i + 1].size() / cells[i].size();
            }

            // Build links between the cells
            cell->visited = false;
            cell->link[Hubward] = Link{ getCell(i - 1, sector / downRatio), false };
            cell->link[RimwardA] = Link{ getCell(i + 1, (sector * upRatio) ), false };
            cell->link[RimwardB] = Link{ getCell(i + 1, (sector * upRatio) + 1), false };
            if (upRatio == 1)
            {
                cell->link[RimwardB] = cell->link[RimwardA];
            }
            cell->link[Turnwise] = Link{ getCell(i, sector + 1), false };
            cell->link[Widdershins] = Link{ getCell(i, sector - 1), false };

        }
    }

    // Random walk - lets just start from the center.
    auto cell = &cells[0][0];
    
    auto NumToVisit =  totalCells;
    while (NumToVisit > 0)
    {
        auto dir = (Direction)(rand() % Direction::DirCount);
        auto adjacent = getAdjacent(cell, (Direction)dir);
        if (adjacent)
        {
            if (adjacent->visited == false)
            {
                // We haven't visited this one, so make the whole back in the direction we walkd
                makeHole(cell, dir);
                NumToVisit--;
                adjacent->visited = true;
            }
            cell = adjacent;
        }
    }

    Pen linePen(Color::DarkRed);
    linePen.SetWidth(2);

    // Degrees -> Radians
    auto ToRad = [](double deg) { return (deg * std::_Pi) / 180;};

    // Draw the walls.  Just draw the hubward and widdershins walls.
    for (int ring = 1; ring < Rings; ring++)
    {
        for (int sector = 0; sector < int(cells[ring].size()); sector++)
        {
            double sectorAngle = 360.0 / cells[ring].size();
            double startSectorAngle = sectorAngle * sector;
            double endSectorAngle = sectorAngle * (sector + 1);

            auto cell = getCell(ring, sector);
            if (!cell->link[Widdershins].hole)
            {
                auto lineStart = CellSectorLength * ring;
                auto lineEnd = (CellSectorLength * (ring + 1)) + 1;
                double startX = lineStart * sin(ToRad(startSectorAngle));
                double startY = lineStart * cos(ToRad(startSectorAngle));
                double endX = lineEnd * sin(ToRad(startSectorAngle));
                double endY = lineEnd * cos(ToRad(startSectorAngle));

                graphics.DrawLine(&linePen,
                    PointF(float(startX + centerBitmapX),
                        float(startY + centerBitmapY)),
                    PointF(float(endX + centerBitmapX),
                        float(endY + centerBitmapY)));
            }

            // Draw an arc
            if (!cell->link[Hubward].hole)
            {
                int len = int(ring * CellSectorLength);
                graphics.DrawArc(&linePen, int(centerBitmapX - len), int(centerBitmapY - len), int(len * 2), int(len * 2), float(startSectorAngle), float(endSectorAngle - startSectorAngle));
            }
        }
    }

    // Draw the outer border
    graphics.DrawEllipse(&linePen, int(centerBitmapX) - int(Radius), int(centerBitmapY) - int(Radius), int(Radius * 2), int(Radius * 2));
}
