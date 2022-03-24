#include "util/terminal.hpp"
#include <iostream>

#ifdef _WIN32
#include <windows.h>
#endif

void util::terminal::clear() {
#ifdef _WIN32
  HANDLE h_stdout = GetStdHandle(STD_OUTPUT_HANDLE);
  COORD coord = {0, 0};
  DWORD count;

  CONSOLE_SCREEN_BUFFER_INFO csbi;
  GetConsoleScreenBufferInfo(h_stdout, &csbi);
  FillConsoleOutputCharacter(h_stdout, ' ', csbi.dwSize.X * csbi.dwSize.Y,
                             coord, &count);
  SetConsoleCursorPosition(h_stdout, coord);
#else
  std::cout << "\x1b[2J\x1b[0;0H";
#endif
}
