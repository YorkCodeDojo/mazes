#include <cstdio>
#include <chrono>
#include "writebitmap.h"
#include "mazes.h"

void main(void* arg, void** args)
{
    // Seed the random generator with time
    srand(int(std::chrono::high_resolution_clock::now().time_since_epoch().count()));

    BinaryTreeMaze();
    RandomWalkMaze();
}

