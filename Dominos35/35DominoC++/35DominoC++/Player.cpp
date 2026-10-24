/*
************************************************************
* Name:  Jared Novack
* Project : 35 Domino C++
* Class : OPL class 01
* Date : 9/20/2026
************************************************************
*/
#include "Player.h"
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>
using namespace std;

/* *********************************************************************
Function Name: getHand
Purpose: To return the player's hand
Parameters:
	none
Return Value: hand
Algorithm:
Reference: none
********************************************************************* */
Hand& Player::getHand()
{
	return hand;
}

/* *********************************************************************
Function Name: setHand
Purpose: To set the player's hand
Parameters:
	sethand, the hand to assign to the player
Return Value: none
Algorithm:
Reference: none
********************************************************************* */
void Player::setHand(Hand sethand)
{
	hand = sethand;
}

/* *********************************************************************
Function Name: getRPoints
Purpose: To return the player's current round score
Parameters:
	none
Return Value: rPoints
Algorithm:
Reference: none
********************************************************************* */
int Player::getRPoints()
{
	return rPoints;
}

/* *********************************************************************
Function Name: addRPoints
Purpose: To add points to the player's current round score
Parameters:
	addrPoints, number of points to add
Return Value: none
Algorithm:
Reference: none
********************************************************************* */
void Player::addRPoints(int addrPoints)
{
	rPoints += addrPoints;
}

/* *********************************************************************
Function Name: resetRPoints
Purpose: To reset the player's current score to zero
Parameters:
	none
Return Value: none
Algorithm:
	1) Sets the player's current score to zero
Reference: none
********************************************************************* */
void Player::resetRPoints()
{
	rPoints = 0;
}

/* *********************************************************************
Function Name: getTPoints
Purpose: To return the player's tournament points
Parameters:
	none
Return Value: tPoints
Algorithm:
Reference: none
********************************************************************* */
int Player::getTPoints()
{
	return tPoints;
}

/* *********************************************************************
Function Name: addTPoints
Purpose: To add points to the player's tournament points
Parameters:
	addtPoints, number of points to add
Return Value: none
Algorithm:
Reference: none
********************************************************************* */
void Player::addTPoints(int addtPoints)
{
	tPoints += addtPoints;
}

/* *********************************************************************
Function Name: convertToTPoints
Purpose: To add the player's current round score to the tournament score
Parameters:
	none
Return Value: none
Algorithm:
	1) Adds the current score to the tournament score
	2) Resets the current score to zero
Reference: none
********************************************************************* */
void Player::convertToTPoints()
{
	tPoints += rPoints;
	resetRPoints();
}

/* *********************************************************************
Function Name: writePlayer
Purpose: To write the player's hand and scores into a .txt file
Parameters:
	file, an ofstream attached to a .txt file
Return Value: none
Algorithm:
	1) Writes the player's hand to the file
	2) Writes the player's current score to the file
	3) Writes the player's tournament score to the file
Reference: none
********************************************************************* */
void Player::writePlayer(ofstream& file)
{
	file << "   Hand:";
	getHand().writeHand(file);

	file << "   Current Score: " << getRPoints() << endl;
	file << "   Tournament Score: " << getTPoints() << endl;
}

/* *********************************************************************
Function Name: readPlayer
Purpose: To read the player's hand and scores from a .txt file
Parameters:
	in, an istream attached to the saved game data
Return Value: none
Algorithm:
	1) Reads the player's hand from the file
	2) Reads the player's current score from the file
	3) Reads the player's tournament score from the file
	4) Sets the player's scores using the values read from the file
Reference: none
********************************************************************* */
void Player::readPlayer(istream& in)
{
	string line;
	string label1, label2;
	int currentScore;
	int tournamentScore;

	getline(in, line);
	istringstream handStream(line);
	hand.readHand(handStream);

	getline(in, line);
	istringstream currentStream(line);
	currentStream >> label1 >> label2 >> currentScore;

	getline(in, line);
	istringstream tournamentStream(line);
	tournamentStream >> label1 >> label2 >> tournamentScore;

	rPoints = currentScore;
	tPoints = tournamentScore;
}