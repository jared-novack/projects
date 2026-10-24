
#ifndef BOARDVIEW_H
#define BOARDVIEW_H

#include "Board.h"

class BoardView
{

public:
    void printBoard(Board board);
    void printUp(Board board);
    void printDown(Board board);
    void spacesToSpinner(Board board);
};

#endif
