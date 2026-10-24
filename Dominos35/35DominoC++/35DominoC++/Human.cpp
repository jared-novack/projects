/*
************************************************************
* Name:  Jared Novack
* Project : 35 Domino C++
* Class : OPL class 01
* Date : 9/20/2026
************************************************************
*/
#include "Human.h"
#include <fstream>
#include <iostream>
using namespace std;

/* *********************************************************************
Function Name: playerTurn
Purpose: To run the human player's turn
Parameters:
	board, the current board state of the game
	deck, the current deck of tiles
Return Value: board/ the updated board state of the game
Algorithm:
	1) Finds all possible moves and optimal moves for the human player's hand
	2) Prompts the user to place a tile, draw a tile, ask for help, or passing
	3) If placing a tile, checks that the tile is in the hand and can be placed on the selected end
	3.1) If a valid tile placement is made, removes the tile from the hand and adds it to the board
	4) If drawing a tile, checks that there are no valid moves and that the deck is not empty before drawing
	5) If help is requested, displays all possible moves and optimal moves
	6) If passing, checks that there are no valid moves and the deck is empty before ending the turn
	7) Continues prompting the user until a valid play, draw, or pass ends the turn
Reference: none
********************************************************************* */
Board Human::playerTurn(Board board, Deck& deck)
{
	char inp, loc;
	vector<int> ends = board.getEnds();
	int num1, num2;
	bool turnEnd = false;
	bool validMove = false;
	Tile temp;
	vector<vector<int>> allMoves = board.getAllMoves(getHand());
	vector<vector<int>> optMoves = board.optimalMoves(getHand());

	if (allMoves.size() > 0)
	{
		validMove = true;
	}
	while (!turnEnd)
	{
		cout << "YOUR HAND: ";
		getHand().printHand();
		cout << endl;

		cout << "You want to place tile (T), draw tile (D), ask for help (H), or pass (P): ";
		cin >> inp;
		if (inp == 'T')
		{
			cout << "Enter the numbers for the amount of pips on each side of the tile (0 to 6): ";
			cin >> num1;

			if (cin.fail())
			{
				cin.clear();
				cin.ignore(1000, '\n');
				cout << "INVALID INPUT ENTERED!" << endl;
				continue;
			}

			cin >> num2;

			if (cin.fail())
			{
				cin.clear();
				cin.ignore(1000, '\n');
				cout << "INVALID INPUT ENTERED!" << endl;
				continue;
			}

			if (num1 < 0 || num1 > 6 || num2 < 0 || num2 > 6)
			{
				cout << "NUMBER OF PIPS LISTED ON A SIDE IS IMPOSSIBLE!" << endl;
			}
			else if(!getHand().hasTile(num1, num2))
			{
				cout << "LISTED TILE NOT IN HAND!" << endl;
				inp = 'q';
			}
			else
			{
				if (board.getHorizontal().size() == 0)
				{
					temp = getHand().removeFromHand(num1, num2);
					board.addFirst(temp);
					cout << "Placed Tile [" << num1 << " - " << num2 << "] as first tile on the board." << endl;
					cout << "Left and Right ends are open!" << endl;

					if (board.getHasSpinner())
					{
						cout << "[" << num1 << " - " << num2 << "] is spinner! Up and Down ends are now open!" << endl;
					}
					board.setPassCheck(false);
					turnEnd = true;
				}
				else if (!board.getHasSpinner())
				{
					cout << "Place it Left (L) or Right (R): ";
					cin >> loc;
					
					if (loc == 'L')
					{
						if (num1 != ends[0] && num2 != ends[0])
						{
							cout << "TILE CANNOT BE PLACED AT LEFT END! NEITHER SIDE HAS " << ends[0] << " PIPS!" << endl;
						}
						else
						{
							temp = getHand().removeFromHand(num1, num2);
							board.addLeft(temp);
							cout << "Placed Tile [" << num1 << " - " << num2 << "] at left end of the board." << endl;

							if (board.getHasSpinner())
							{
								cout << "[" << num1 << " - " << num2 << "] is spinner! Up and Down ends are now open!" << endl;
							}
							board.setPassCheck(false);
							turnEnd = true;
						}
					}
					else if (loc == 'R')
					{
						if (num1 != ends[1] && num2 != ends[1])
						{
							cout << "TILE CANNOT BE PLACED AT RIGHT END! NEITHER SIDE HAS " << ends[1] << " PIPS!" << endl;
						}
						else
						{
							temp = getHand().removeFromHand(num1, num2);
							board.addRight(temp);
							cout << "Placed Tile [" << num1 << " - " << num2 << "] at right end of the board." << endl;

							if (board.getHasSpinner())
							{
								cout << "[" << num1 << " - " << num2 << "] is spinner! Up and Down ends are now open!" << endl;
							}
							board.setPassCheck(false);
							turnEnd = true;
						}
					}
					else
					{
						cout << "INVALID DIRECTION ENTERED!" << endl;
					}
				}
				else
				{
					cout << "Place it Left (L), Right (R), Up (U), or Down (D): ";
					cin >> loc;

					if (loc == 'L')
					{
						if (num1 != board.getHorizontal()[0].getNum1() && num2 != board.getHorizontal()[0].getNum1())
						{
							cout << "TILE CANNOT BE PLACED AT LEFT END! NEITHER SIDE HAS " << board.getHorizontal()[0].getNum1() << " PIPS!" << endl;
						}
						else
						{
							temp = getHand().removeFromHand(num1, num2);
							board.addLeft(temp);
							cout << "Placed Tile [" << num1 << " - " << num2 << "] at left end of the board." << endl;
							board.setPassCheck(false);
							turnEnd = true;
						}
					}
					else if (loc == 'R')
					{
						if (num1 != board.getHorizontal()[board.getHorizontal().size() - 1].getNum2() && num2 != board.getHorizontal()[board.getHorizontal().size() - 1].getNum2())
						{
							cout << "TILE CANNOT BE PLACED AT RIGHT END! NEITHER SIDE HAS " << board.getHorizontal().size() << " PIPS!" << endl;
						}
						else
						{
							temp = getHand().removeFromHand(num1, num2);
							board.addRight(temp);
							cout << "Placed Tile [" << num1 << " - " << num2 << "] at right end of the board." << endl;
							board.setPassCheck(false);
							turnEnd = true;
						}
					}
					else if (loc == 'U')
					{
						if (num1 != ends[2] && num2 != ends[2])
						{
							cout << "TILE CANNOT BE PLACED AT UP END! NEITHER SIDE HAS " << ends[2] << " PIPS!" << endl;
						}
						else
						{
							temp = getHand().removeFromHand(num1, num2);
							board.addUp(temp);
							cout << "Placed Tile [" << num1 << " - " << num2 << "] at up end of the board." << endl;
							board.setPassCheck(false);
							turnEnd = true;
						}
					}
					else if (loc == 'D')
					{
						if (num1 != ends[3] && num2 != ends[3])
						{
							cout << "TILE CANNOT BE PLACED AT DOWN END! NEITHER SIDE HAS " << ends[3] << " PIPS!" << endl;
						}
						else
						{
							temp = getHand().removeFromHand(num1, num2);
							board.addDown(temp);
							cout << "Placed Tile [" << num1 << " - " << num2 << "] at down end of the board." << endl;
							board.setPassCheck(false);
							turnEnd = true;
						}
					}
					else
					{
						cout << "INVALID DIRECTION ENTERED!" << endl;
					}
				}
			}
		}
		else if (inp == 'D')
		{
			if (allMoves.size() != 0)
			{
				cout << "YOU HAVE A VALID PLAY! YOU CANNOT DRAW!" << endl;
			}
			else if (deck.getDeckSize() == 0)
			{
				cout << "DECK IS EMPTY! YOU CANNOT DRAW!" << endl;
			}
			else
			{
				temp = deck.drawFromDeck();
				getHand().addToHand(temp);
				cout << "Tile [" << temp.getNum1() << " - " << temp.getNum2() << "] drawn" << endl;
				cout << "TILES IN DECK: " << deck.getDeckSize() << endl;

				allMoves = board.getAllMoves(getHand());
				optMoves = board.optimalMoves(getHand());

				if (allMoves.size() > 0)
				{
					validMove = true;
				}
			}
		}
		else if (inp == 'H')
		{
			if (allMoves.size() == 0)
			{
				cout << "You have no valid moves. You must draw or pass the turn." << endl;
			}
			else
			{
				cout << "ALL MOVES:" << endl;
				cout << "TileNum1  TileNum2  End  BoardSum  Score" << endl;

				for (int i = 0; i < allMoves.size(); i++)
				{
					cout << allMoves[i][0] << "         " << allMoves[i][1] << "         ";

					if (allMoves[i][2] == 0) 
					{
						cout << "L";
					}
					else if (allMoves[i][2] == 1)
					{
						cout << "R";
					}
					else if (allMoves[i][2] == 2)
					{
						cout << "U";
					}
					else if (allMoves[i][2] == 3)
					{
						cout << "D";
					}
					else
					{
						cout << "C";
					}

					cout << "    " << allMoves[i][3] << "         " << allMoves[i][4] << endl;
				}

				cout << endl;
				cout << "OPTIMAL MOVES:" << endl;
				cout << "TileNum1  TileNum2  End  BoardSum  Score" << endl;

				for (int i = 0; i < optMoves.size(); i++)
				{
					cout << optMoves[i][0] << "         " << optMoves[i][1] << "         ";

					if (optMoves[i][2] == 0)
					{
						cout << "L";
					}
					else if (optMoves[i][2] == 1)
					{
						cout << "R";
					}
					else if (optMoves[i][2] == 2)
					{
						cout << "U";
					}
					else if (optMoves[i][2] == 3)
					{
						cout << "D";
					}
					else
					{
						cout << "C";
					}

					cout << "    " << optMoves[i][3] << "         " << optMoves[i][4] << endl;
				}

				cout << endl;
			}
		}
		else if (inp == 'P')
		{
			if (allMoves.size() != 0)
			{
				cout << "YOU HAVE A VALID PLAY! YOU CANNOT DRAW!" << endl;
			}
			else if (deck.getDeckSize() != 0)
			{
				cout << "DECK IS NOT EMPTY! YOU CAN DRAW! YOU MUST PASS!" << endl;
			}
			else
			{
				cout << "You pass the turn" << endl;
				board.setPassCheck(true);
				turnEnd = true;
			}
		}
		else
		{
			cout << "INVALID INPUT" << endl;
		}
	}


	return board;

}