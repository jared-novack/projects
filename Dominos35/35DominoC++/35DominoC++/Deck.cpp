/*
************************************************************
* Name:  Jared Novack
* Project : 35 Domino C++
* Class : OPL class 01
* Date : 9/20/2026
************************************************************
*/
#include "Deck.h"
#include <fstream>
#include <random>
#include <iostream>
using namespace std;

/* *********************************************************************
Function Name: getDeck
Purpose: To return the current deck of tiles
Parameters:
	none
Return Value: deck/ vector containing the tiles in the deck
Algorithm:
Reference: none
********************************************************************* */
vector<Tile> Deck::getDeck()
{
	return deck;
}

/* *********************************************************************
Function Name: addToDeck
Purpose: To add a tile to the deck
Parameters:
	tile, tile to add to the deck
Return Value: none
Algorithm:
	1) Adds the tile to the end of the deck
Reference: none
********************************************************************* */
void Deck::addToDeck(Tile tile)
{
	deck.push_back(tile);
}

/* *********************************************************************
Function Name: getDeckSize
Purpose: To return the current number of tiles in the deck
Parameters:
	none
Return Value: deck.size()/ the number of tiles in the deck
Algorithm:
Reference: none
********************************************************************* */
int Deck::getDeckSize()
{
	return deck.size();
}

/* *********************************************************************
Function Name: isEmpty
Purpose: To check whether the deck is empty
Parameters:
	none
Return Value: true if the deck is empty, otherwise false
Algorithm:
	1) If statement to check if the deck size is 0
	2) Returns true if the deck is empty and false otherwise
Reference: none
********************************************************************* */
bool Deck::isEmpty()
{
	if (deck.size() == 0)
	{
		return true;
	}
	else
	{
		return false;
	}
}

/* *********************************************************************
Function Name: toBottomDeck
Purpose: To add a tile to the bottom of the deck
Parameters:
	tile, tile to add to the bottom of the deck
Return Value: none
Algorithm:
	1) Inserts the tile at the beginning of the deck
Reference: none
********************************************************************* */
void Deck::toBottomDeck(Tile tile)
{
	deck.push_back(tile);
}

/* *********************************************************************
Function Name: drawFromDeck
Purpose: To remove and return a tile from the top of the deck
Parameters:
	none
Return Value: tempTile/ the tile drawn from the deck
Algorithm:
	1) Stores the last tile in the deck
	2) Removes the last tile from the deck
	3) Returns the removed tile
Reference: none
********************************************************************* */
Tile Deck::drawFromDeck()
{
	Tile tempTile;
	tempTile = deck[0];
	deck.erase(deck.begin());
	return tempTile;
}

/* *********************************************************************
Function Name: removeFromDeck
Purpose: To find and remove a specific tile from the deck
Parameters:
	num1, number of pips on one side of tile
	num2, number of pips on the other side of tile
Return Value: tempTile/ the removed tile
Algorithm:
	1) For loop to check each tile in the deck
	2) If statement to check if the tile matches the given numbers
	3) Removes the matching tile from the deck and returns it
Reference: none
********************************************************************* */
Tile Deck::removeFromDeck(int num1, int num2)
{
	Tile tempTile;

	for (int i = 0; i < deck.size(); i++)
	{
		if (deck[i].checkTile(num1, num2))
		{
			tempTile = deck[i];
			deck.erase(deck.begin() + i);
			break;
		}
	}

	return tempTile;
}

/* *********************************************************************
Function Name: printDeck
Purpose: To print all of the tiles currently in the deck (not used in game but for testing)
Parameters:
	none
Return Value: none
Algorithm:
	1) For loop to go through each tile in the deck
	2) Calls printTile to print each tile
Reference: none
********************************************************************* */
void Deck::printDeck()
{
	for (int i = 0; i < deck.size(); i++)
	{
		deck[i].printTile();
		cout << endl;
	}
}

/* *********************************************************************
Function Name: shuffleDeck
Purpose: To randomly shuffle the tiles in the deck
Parameters:
	none
Return Value: none
Algorithm:
	1) Uses random_shuffle to randomly rearrange the tiles in the deck
Reference: none
********************************************************************* */
void Deck::shuffleDeck()
{
	random_shuffle(deck.begin(), deck.end());
}

/* *********************************************************************
Function Name: startHandDraw
Purpose: To draw the starting number of tiles from the deck into a hand (commented out 1 for testing)
Parameters:
	hand, hand that will receive the drawn tiles
Return Value: none
Algorithm:
	1) For loop to draw 7 tiles from the deck to add to the hand
Reference: none
********************************************************************* */
void Deck::startHandDraw(Hand& hand)
{
	Tile temp;
	for (int i = 0; i < /*1*/7; i++)
	{
		temp = drawFromDeck();
		hand.addToHand(temp);
	}
}

/* *********************************************************************
Function Name: restockDeck
Purpose: To return the tiles from the players' hands and board to the deck
Parameters:
	h1, first player's hand
	h2, second player's hand
	b, current board containing played tiles
Return Value: none
Algorithm:
	1) Removes all tiles from the first player's hand and adds them to the deck
	2) Removes all tiles from the second player's hand and adds them to the deck
	3) Removes all tiles from the horizontal board line and adds them to the deck
	3.1) Removes the spinner status from the spinner tile before adding it to the deck
	4) Removes all non-spinner tiles from the vertical board line and adds them to the deck
Reference: none
********************************************************************* */
void Deck::restockDeck(Hand& h1, Hand& h2, Board& b)
{
	while (!h1.isEmpty())
	{
		deck.push_back(h1.removeFromHand());
	}

	while (!h2.isEmpty())
	{
		deck.push_back(h2.removeFromHand());
	}

	while (b.getHorizontal().size() != 0)
	{
		Tile temp = b.removeFromHorizontal();
		if (temp.getSpinner() == true)
		{
			temp.setSpinner(false);
		}
		deck.push_back(temp);
	}

	while (b.getVertical().size() != 0)
	{
		Tile temp = b.removeFromVertical();
		if (temp.getSpinner() == false)
		{
			deck.push_back(temp);
		}

	}

	b.setSpinNum(-1);
}

/* *********************************************************************
Function Name: newDeck
Purpose: To create a new deck containing all possible domino tiles
Parameters:
	none
Return Value: none
Algorithm:
	1) For loops to create every combination of tiles from 0-0 through 6-6
	2) Adds each created tile to the deck
Reference: none
********************************************************************* */
void Deck::newDeck()
{
	for (int i = 0; i <= 6; i++)
	{
		for (int j = i; j <= 6; j++)
		{
			Tile t;
			t.setTile(i, j);
			deck.push_back(t);
		}
	}
}

/* *********************************************************************
Function Name: writeDeck
Purpose: To write the current deck of tiles into a .txt file
Parameters:
	file, an ofstream attached to a .txt file
Return Value: none
Algorithm:
	1) Outputs the Boneyard label to the file
	2) For loop to write each tile in the deck to the file
Reference: none
********************************************************************* */
void Deck::writeDeck(ofstream& file)
{
	file << "Boneyard: ";

	for (int i = 0; i < deck.size(); i++)
	{
		deck[i].writeTile(file);
	}
	file << endl;
}

/* *********************************************************************
Function Name: readDeck
Purpose: To read the deck of tiles from a .txt file into the deck vector
Parameters:
	in, an istream attached to the saved game data
Return Value: none
Algorithm:
	1) Reads the Boneyard label from the file
	2) While loop to read each tile from the file
	3) Adds each tile that is successfully read to the deck
Reference: none
********************************************************************* */
void Deck::readDeck(istream& in)
{
	string label;
	in >> label;

	while (in)
	{
		Tile tile;
		tile.readTile(in);

		if (in)
		{
			deck.push_back(tile);
		}
	}
}