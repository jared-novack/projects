/*
************************************************************
* Name:  Jared Novack
* Project : 35 Domino C++
* Class : OPL class 01
* Date : 9/20/2026
************************************************************
*/
#include "Computer.h"
#include <fstream>
#include <iostream>
using namespace std;


/* *********************************************************************
Function Name: playerTurn
Purpose: To run the computer's turn
Parameters:
	board, the current board state of the game
	deck, the current deck of tiles
Return Value: board/ the updated board state after the computer's turn
Algorithm:
	1) Find all optimal moves that the computer can make
	2) If there are multiple optimal moves, randomly select one
	3) If a valid move is selected, remove the tile from the computer's hand and place it on the board
	4) If there are no valid moves and tiles remain in the deck, draw a tile and check for possible moves again
	5) If there are no valid moves and the deck is empty, pass the computer's turn
Reference: none
********************************************************************* */
Board Computer::playerTurn(Board board, Deck& deck)
{
	srand(time(0));
	Tile temp;
	bool turnEnd = false;
	bool validMove = false;
	vector<vector<int>> optMoves = board.optimalMoves(getHand());

	if (optMoves.size() > 0)
	{
		validMove = true;
	}

	if (board.getHorizontal().size() == 0)
	{
		int randomChoice = rand() % optMoves.size();

		temp = getHand().removeFromHand(optMoves[randomChoice][0], optMoves[randomChoice][1]);
		board.addFirst(temp);
		cout << "Placed Tile [" << optMoves[randomChoice][0] << " - " << optMoves[randomChoice][1] << "] as first tile on the board." << endl;
		if (board.getHasSpinner())
		{
			cout << "[" << optMoves[randomChoice][0] << " - " << optMoves[randomChoice][1] << "] is spinner! Up and Down ends are now open!" << endl;
		}
		board.setPassCheck(false);
		turnEnd = true;
	}

	while (!turnEnd)
	{
		if (validMove == true)
		{
			int randomChoice = rand() % optMoves.size();
			if (optMoves[randomChoice][2] == 0)
			{
				temp = getHand().removeFromHand(optMoves[randomChoice][0], optMoves[randomChoice][1]);
				board.addLeft(temp);
				cout << "Placed Tile [" << optMoves[randomChoice][0] << " - " << optMoves[randomChoice][1] << "] at left end of the board." << endl;
				board.setPassCheck(false);
				turnEnd = true;
			}
			else if (optMoves[randomChoice][2] == 1)
			{
				temp = getHand().removeFromHand(optMoves[randomChoice][0], optMoves[randomChoice][1]);
				board.addRight(temp);
				cout << "Placed Tile [" << optMoves[randomChoice][0] << " - " << optMoves[randomChoice][1] << "] at right end of the board." << endl;
				board.setPassCheck(false);
				turnEnd = true;
			}
			else if (optMoves[randomChoice][2] == 2 && board.getHasSpinner())
			{
				temp = getHand().removeFromHand(optMoves[randomChoice][0], optMoves[randomChoice][1]);
				board.addUp(temp);
				cout << "Placed Tile [" << optMoves[randomChoice][0] << " - " << optMoves[randomChoice][1] << "] at up end of the board." << endl;
				board.setPassCheck(false);
				turnEnd = true;
			}
			else if(board.getHasSpinner())
			{
				temp = getHand().removeFromHand(optMoves[randomChoice][0], optMoves[randomChoice][1]);
				board.addDown(temp);
				cout << "Placed Tile [" << optMoves[randomChoice][0] << " - " << optMoves[randomChoice][1] << "] at down end of the board." << endl;
				board.setPassCheck(false);
				turnEnd = true;
			}

		}
		else
		{
			if (deck.getDeckSize() != 0)
			{
				temp = deck.drawFromDeck();
				getHand().addToHand(temp);
				cout << "Computer drew a tile due to no valid play." << endl;

				optMoves = board.optimalMoves(getHand());

				if (optMoves.size() > 0)
				{
					validMove = true;
				}
			}
			else
			{
				cout << "Computer passes their turn due to no valid play and no tiles left to draw." << endl;
				board.setPassCheck(true);
				turnEnd = true;
			}
		}
	}

	return board;

}