/*
************************************************************
* Name:  Jared Novack
* Project : 35 Domino C++
* Class : OPL class 01
* Date : 9/20/2026
************************************************************
*/
#include <iostream>
#include <cstdlib>
#include <ctime>
#include <algorithm>
#include "Board.h"
#include "BoardView.h"
#include "Tile.h"
#include "Round.h"

/* *********************************************************************
Function Name: main
Purpose: To run the game
Parameters:
    none
Return Value: none
Algorithm:
Reference: none
********************************************************************* */
int main()
{
   srand(time(NULL));

   Round round;
   bool begin = round.startGame();
   while (true)
   {
       if (begin)
       {
           round.startRound();
       }
       else
       {
           begin = true;
       }
       if (round.playingGame())
       {
           if (round.endRound() == true)
           {
               break;
           }
       }
       else
       {
           break;
       }
   }
}
