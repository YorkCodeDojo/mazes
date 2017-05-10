#include <windows.h>
#include <objidl.h>
#include <gdiplus.h>
#include <memory>

using namespace Gdiplus;
#pragma comment (lib,"Gdiplus.lib")

std::shared_ptr<Bitmap> spBitmap;
VOID OnPaint(HDC hdc)
{
    Graphics graphics(hdc);
    graphics.SetSmoothingMode(SmoothingMode::SmoothingModeHighQuality);

    graphics.Clear(Color::WhiteSmoke);

    // Lock the bitmap with the same format, to avoid translation, draw a green line on it, and copy it to the screen.
    if (spBitmap)
    {
        BitmapData data;
        spBitmap->LockBits(nullptr, ImageLockModeRead | ImageLockModeWrite, PixelFormat32bppPARGB, &data);

        for (UINT y = 0; y < data.Height; y++)
        {
            auto x = y; //for (auto x = 0; x < data.Width; x++)
            {
                // Pixel Maths.... step down by the x stride to the row, then across to the pixel
                // We work in 4 byte pieces, because each pixel is ARGB
                uint32_t* pPixel = (uint32_t*)((uint8_t*)data.Scan0 + (y * data.Stride) + (x * 4));
                *pPixel = 0xFF00FF00;
            }
        }
        spBitmap->UnlockBits(&data);
        graphics.DrawImage(spBitmap.get(), 0, 0);
    }
    Pen linePen(Color::DarkRed);
    linePen.SetWidth(2);

    //Bitmap bmp(100, 100, &graphics);
    //bmp.LockBits(nullptr, 0, )
    //graphics.Draw
    graphics.DrawLine(&linePen, 10, 10, 200, 200);
    graphics.DrawRectangle(&linePen, 100, 10, 200, 200);
}

LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);

INT WINAPI WinMain(HINSTANCE hInstance, HINSTANCE, PSTR, INT iCmdShow)
{
    HWND                hWnd;
    MSG                 msg;
    WNDCLASS            wndClass;
    GdiplusStartupInput gdiplusStartupInput;
    ULONG_PTR           gdiplusToken;

    // Initialize GDI+.
    GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);

    wndClass.style = CS_HREDRAW | CS_VREDRAW;
    wndClass.lpfnWndProc = WndProc;
    wndClass.cbClsExtra = 0;
    wndClass.cbWndExtra = 0;
    wndClass.hInstance = hInstance;
    wndClass.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wndClass.hCursor = LoadCursor(NULL, IDC_ARROW);
    wndClass.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
    wndClass.lpszMenuName = NULL;
    wndClass.lpszClassName = TEXT("GettingStarted");

    RegisterClass(&wndClass);

    hWnd = CreateWindow(
        TEXT("GettingStarted"),   // window class name
        TEXT("Getting Started"),  // window caption
        WS_OVERLAPPEDWINDOW,      // window style
        CW_USEDEFAULT,            // initial x position
        CW_USEDEFAULT,            // initial y position
        CW_USEDEFAULT,            // initial x size
        CW_USEDEFAULT,            // initial y size
        NULL,                     // parent window handle
        NULL,                     // window menu handle
        hInstance,                // program instance handle
        NULL);                    // creation parameters

    ShowWindow(hWnd, iCmdShow);
    UpdateWindow(hWnd);

    while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    GdiplusShutdown(gdiplusToken);
    return int(msg.wParam);
}  // WinMain

LRESULT CALLBACK WndProc(HWND hWnd, UINT message,
    WPARAM wParam, LPARAM lParam)
{
    HDC          hdc;
    PAINTSTRUCT  ps;

    switch (message)
    {
    case WM_SIZE:
        spBitmap = std::make_shared<Bitmap>(LOWORD(lParam), HIWORD(lParam), PixelFormat32bppPARGB);
        return 0;
    case WM_PAINT:
        hdc = BeginPaint(hWnd, &ps);
        OnPaint(hdc);
        EndPaint(hWnd, &ps);
        return 0;
    case WM_DESTROY:
        spBitmap.reset();
        PostQuitMessage(0);
        return 0;
    default:
        return DefWindowProc(hWnd, message, wParam, lParam);
    }
} // WndProc


