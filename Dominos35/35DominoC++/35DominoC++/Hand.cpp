/*
************************************************************
* Name:  Jared Novack
* Project : 35 Domino C++
* Class : OPL class 01
* Date : 9/20/2026
************************************************************
*/
#include "Hand.h"
#include <fstream>
#include <iostream>
using namespace std;

/* *********************************************************************
Function Name: getHand
Purpose: To return the players hand
Parameters:
	none
Return Value: playerHand/ vector containing the tiles in the hand
Algorithm:
Reference: none
********************************************************************* */
vector<Tile> Hand::getHand()
{
	return playerHand;
}

/* *********************************************************************
Function Name: getHandSum
Purpose: To return the total value of the tiles in the player's hand
Parameters:
	none
Return Value: handSum
Algorithm:
Reference: none
********************************************************************* */
int Hand::getHandSum()
{
	return handSum;
}

/* *********************************************************************
Function Name: getHandSize
Purpose: To return the number of tiles in the player's hand
Parameters:
	none
Return Value: playerHand.size()/ the number of tiles in the hand
Algorithm:
Reference: none
********************************************************************* */
int Hand::getHandSize()
{
	return playerHand.size();
}


/* *********************************************************************
Function Name: isEmpty
Purpose: To check whether the player's hand is empty
Parameters:
	none
Return Value: true if the hand is empty, otherwise false
Algorithm:
	1) If statement to check if the hand size is 0
	2) Returns true if the hand is empty and false otherwise
Reference: none
********************************************************************* */
bool Hand::isEmpty()
{
	if (playerHand.size() == 0)
	{
		return true;
	}
	else
	{
		return false;
	}
}

/* *********************************************************************
Function Name: addToHand
Purpose: To add a tile to the player's hand
Parameters:
	tile, tile to add to the hand
Return Value: none
Algorithm:
	1) Adds the tile to the end of the player's hand
Reference: none
********************************************************************* */
void Hand::addToHand(Tile tile)
{
	playerHand.push_back(tile);
}

/* *********************************************************************
Function Name: hasTile
Purpose: To check if the player's hand contains a specific tile
Parameters:
	num1, number of pips on one side of tile
	num2, number of pips on the other side of tile
Return Value: true if the tile is in the hand, otherwise false
Algorithm:
	1) For loop to check each tile in the player's hand
	2) If statement to check if the tile matches the given numbers
	2.1) Returns true if a matching tile is found
	3) Returns false if no matching tile is found
Reference: none
********************************************************************* */
bool Hand::hasTile(int num1, int num2)
{
	for (int i = 0; i < playerHand.size(); i++)
	{
		if (playerHand[i].checkTile(num1, num2))
		{
			return true;
		}
	}

	return false;
}

/* *********************************************************************
Function Name: removeFromHand
Purpose: To remove and return the last tile from the player's hand
Parameters:
	none
Return Value: tempTile/ the removed tile
Algorithm:
	1) Stores the last tile in the player's hand
	2) Removes the last tile from the player's hand
	3) Returns the removed tile
Reference: none
********************************************************************* */
Tile Hand::removeFromHand()
{
	Tile tempTile;
	tempTile = playerHand[playerHand.size()-1];
	playerHand.pop_back();
	return tempTile;
}

/* *********************************************************************
Function Name: removeFromHand
Purpose: To find and remove a specific tile from the player's hand
Parameters:
	num1, number of pips on one side of tile
	num2, number of pips on the other side of tile
Return Value: tempTile/ the removed tile
Algorithm:
	1) For loop to check each tile in the player's hand
	2) If statement to check if the tile matches the given numbers
	3) Removes the matching tile from the hand and returns it
Reference: none
********************************************************************* */
Tile Hand::removeFromHand(int num1, int num2)
{
	Tile tempTile;

	for (int i = 0; i < playerHand.size(); i++)
	{
		if (playerHand[i].checkTile(num1, num2))
		{
			tempTile = playerHand[i];
			playerHand.erase(playerHand.begin() + i);
			break;
		}
	}

	return tempTile;
}

/* *********************************************************************
Function Name: updateHandSum
Purpose: To calculate the total value of all tiles in the player's hand
Parameters:
	none
Return Value: none
Algorithm:
	1) Sets the hand sum to 0
	2) For loop to go through each tile in the player's hand
	3) Adds the value of each tile to the hand sum
Reference: none
********************************************************************* */
void Hand::updateHandSum()
{
	handSum = 0;

	for (int i = 0; i < playerHand.size(); i++)
	{
		playerHand[i].setSum();
		handSum += playerHand[i].getSum();
	}
}

/* *********************************************************************
Function Name: printHand
Purpose: To print all of the tiles currently in the player's hand
Parameters:
	none
Return Value: none
Algorithm:
	1) For loop to go through each tile in the player's hand
	2) Calls printTile to print each tile
	3) Prints a comma between each tile
Reference: none
********************************************************************* */
void Hand::printHand()
{

	for (int i = 0; i < playerHand.size(); i++)
	{
		playerHand[i].printTile();
		if ((i + 1) != playerHand.size())
		{
			cout << ", ";
		}

	}
}

/* *********************************************************************
Function Name: writeHand
Purpose: To write the player's hand into a .txt file
Parameters:
	file, an ofstream attached to a .txt file
Return Value: none
Algorithm:
	1) For loop to go through each tile in the player's hand
	2) Writes each tile to the file
	3) Moves to the next line after all tiles have been written
Reference: none
********************************************************************* */
void Hand::writeHand(ofstream& file)
{
	for (int i = 0; i < playerHand.size(); i++)
	{
		playerHand[i].writeTile(file);
	}
	file << endl;
}

/* *********************************************************************
Function Name: readHand
Purpose: To read the player's hand from a .txt file into the hand vector
Parameters:
	in, an istream attached to the saved game data
Return Value: none
Algorithm:
	1) Reads the hand label from the file
	2) While loop to read each tile from the file
	3) Adds each tile that is successfully read to the player's hand
Reference: none
********************************************************************* */
void Hand::readHand(istream& in)
{
	string label;
	in >> label;

	while (in)
	{
		Tile tile;
		tile.readTile(in);

		if (in)
		{
			playerHand.push_back(tile);
		}
	}
}