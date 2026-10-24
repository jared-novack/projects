/*
************************************************************
* Name:  Jared Novack
* Project : 35 Domino C++
* Class : OPL class 01
* Date : 9/20/2026
************************************************************
*/
#include "Tournament.h"
#include <iostream>
using namespace std;

/* *********************************************************************
Function Name: getTScore
Purpose: To return the tournament score needed to end the tournament
Parameters:
	none
Return Value: tScore
Algorithm:
Reference: none
********************************************************************* */
int Tournament::getTScore()
{
	return tScore;
}

/* *********************************************************************
Function Name: setTScore
Purpose: To set the tournament score needed to end the tournament
Parameters:
	setTScore, the score needed to end the tournament
Return Value: none
Algorithm:
Reference: none
********************************************************************* */
void Tournament::setTScore(int setTScore)
{
	tScore = setTScore;
}

/* *********************************************************************
Function Name: checkTornamentEnd
Purpose: To check whether the either player has reached the ending score
Parameters:
	human, the human player
	computer, the computer player
Return Value: true if either player has reached the tournament score, otherwise false
Algorithm:
	1) Checks if both players tournament scores are below the ending score
	2) Returns false if both players are below the ending score
	3) Returns true if either player has reached or exceeded the ending score
Reference: none
********************************************************************* */
bool Tournament::checkTornamentEnd(Human human, Computer computer)
{
	if (human.getTPoints() < tScore && computer.getTPoints() < tScore)
	{
		return false;
	
	}
	else
	{
		return true;
	}
}

/* *********************************************************************
Function Name: checkWinner
Purpose: To determine and display the tournament winner
Parameters:
	human, the human player
	computer, the computer player
Return Value: none
Algorithm:
	1) Compares the human player's tournament score with the computer player's tournament score
	2) If the human player's score is higher, displays that the human player won
	3) If the computer player's score is higher, displays that the human player lost
	4) If the scores are equal, displays that the tournament ended in a draw
Reference: none
********************************************************************* */
void Tournament::checkWinner(Human human, Computer computer)
{
	if (human.getTPoints() > computer.getTPoints())
	{
		cout << "YOU WIN: " << human.getTPoints() << " to " << computer.getTPoints() << endl;
	}
	else if (human.getTPoints() < computer.getTPoints())
	{
		cout << "YOU LOSE: " << human.getTPoints() << " to " << computer.getTPoints() << endl;
	}
	else
	{
		cout << "DRAW: " << human.getTPoints() << " to " << computer.getTPoints() << endl;
	}
}