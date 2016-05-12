#include <fstream>
#include <cstdio>
#include <iostream>
#include <chrono>

#include "writebitmap.h"

void main(void* arg, void** args)
{
    Color col;
   
    const int BitmapWidth = 512;
    const int BitmapHeight = 512;
 
    Bitmap* pBitmap = CreateBitmap(BitmapWidth, BitmapHeight);

    col.blue = 255;
    col.green = 255;
    col.red = 255;
    ClearBitmap(pBitmap, col);

    col.red = 0;
    col.green = 0;
    col.blue = 255;
    DrawLine(pBitmap, 0, 0, 256, 256, col);

    col.red = 255;
    PutPixel(pBitmap, 300, 300, col);

    col.green = 255;
    col.red = 0;

    DrawCircle(pBitmap, 400, 400, 80, col);

    col.red = 0;
    col.green = 0;
    DrawArc(pBitmap, 400, 400, 90, 0, 45, col);

    col.red = 128;
    DrawBlock(pBitmap, 25, 25, 55, 55, col);

    WriteBitmap(pBitmap, "maze.bmp");

    DestroyBitmap(pBitmap);

    system("start maze.bmp");
}
