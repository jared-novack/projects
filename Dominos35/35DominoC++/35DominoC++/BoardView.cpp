/*
************************************************************
* Name:  Jared Novack
* Project : 35 Domino C++
* Class : OPL class 01
* Date : 9/20/2026
************************************************************
*/
#include "BoardView.h"
#include <iostream>
#include <vector>
using namespace std;


/* *********************************************************************
Function Name: printBoard
Purpose: To print the visual representation of the game board
Parameters:
    board, Board containing the tiles and positions to be printed
Return Value: none
Algorithm:
    1) If a vertical line exists, calls printUp to print the tiles above the horizontal line
    2) For loop to print each tile in the horizontal line
    3) If a vertical line exists, calls printDown to print the tiles below the horizontal line
Reference: none
********************************************************************* */
void BoardView::printBoard(Board board)
{
    cout << "GAME BOARD: " << endl;

    if (board.getVertical().size() > 0)
    {
        printUp(board);
    }


    cout << "L ";
    for (int i = 0; i < board.getHorizontal().size(); i++)
    {
        if (board.getHorizontal()[i].getDouble() == false)
        {
            cout << board.getHorizontal()[i].getNum1() << "-" << board.getHorizontal()[i].getNum2() << " ";
        }
        else
        {
            cout << board.getHorizontal()[i].getNum1() << " ";
        }
    }
    cout << "R " << endl;

    if (board.getVertical().size() > 0)
    {
        printDown(board);
    }


}

/* *********************************************************************
Function Name: printUp
Purpose: To print the vertical tiles above the horizontal board line
Parameters:
    board, Board containing the tiles and spinner position to be printed
Return Value: none
Algorithm:
    1) Calls spacesToSpinner to position the vertical tiles above the spinner
    2) For loop to print each vertical tile above the spinner
    3) Last two for loops to print the spinner
Reference: none
********************************************************************* */
void BoardView::printUp(Board board)
{
    spacesToSpinner(board);
    cout << "U " << endl;

    for (int i = 0; i < board.getVerSpinnerLoc(); i++)
    {
        spacesToSpinner(board);
        cout << board.getVertical()[i].getNum1() << endl;
        spacesToSpinner(board);
        cout << "|" << endl;
        spacesToSpinner(board);
        cout << board.getVertical()[i].getNum2() << endl;
    }

    cout << "  ";
    for (int i = 0; i < board.getHorizontal().size(); i++)
    {
        if (board.getHorizontal()[i].getDouble() == true)
        {
            cout << board.getHorizontal()[i].getNum1() << " ";
        }
        else
        {
            cout << "    ";
        }
    }
    cout << endl;

    cout << "  ";
    for (int i = 0; i < board.getHorizontal().size(); i++)
    {
        if (board.getHorizontal()[i].getDouble() == true)
        {
            cout << "| ";
        }
        else
        {
            cout << "    ";
        }
    }
    cout << endl;
}

/* *********************************************************************
Function Name: printDown
Purpose: To print the vertical tiles below the horizontal board line
Parameters:
    board, Board containing the vertical tiles and spinner position
Return Value: none
Algorithm:
    1) For loop to start after the spinner and print each vertical tile below it
    2) Calls spacesToSpinner to position the vertical tiles below the spinner
    3) Prints the down label after all vertical tiles have been printed
Reference: none
********************************************************************* */
void BoardView::printDown(Board board)
{
    for (int i = board.getVerSpinnerLoc()+1; i < board.getVertical().size(); i++)
    {
        spacesToSpinner(board);
        cout << board.getVertical()[i].getNum1() << endl;
        spacesToSpinner(board);
        cout << "|" << endl;
        spacesToSpinner(board);
        cout << board.getVertical()[i].getNum2() << endl;
    }

    spacesToSpinner(board);
    cout << "D " << endl;

}

/* *********************************************************************
Function Name: spacesToSpinner
Purpose: To print the spaces needed to align the vertical line with the spinner
Parameters:
    board, Board containing the horizontal tiles and spinner position
Return Value: none
Algorithm:
    1) For loop to go through the horizontal tiles before the spinner
    2) If a tile is a double, prints two spaces
    3) Otherwise, prints four spaces to maintain the board alignment
Reference: none
********************************************************************* */
void BoardView::spacesToSpinner(Board board) 
{
    cout << "  ";
    for (int i = 0; i < board.getHorSpinnerLoc(); i++)
    {
        if (board.getHorizontal()[i].getDouble() == true)
        {
            cout << "  ";
        }
        else
        {
            cout << "    ";
        }
    }
}
