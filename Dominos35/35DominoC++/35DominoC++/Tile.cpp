/*
************************************************************
* Name:  Jared Novack
* Project : 35 Domino C++
* Class : OPL class 01
* Date : 9/20/2026
************************************************************
*/
#include "Tile.h"
#include <fstream>
#include <iostream>
using namespace std;

/* *********************************************************************
Function Name: getNum1
Purpose: To return the first number of the tile
Parameters:
	none
Return Value: num1
Algorithm:
Reference: none
********************************************************************* */
int Tile::getNum1()
{
	return num1;
}

/* *********************************************************************
Function Name: setNum1
Purpose: To set the first number of the tile
Parameters:
	setnum1, the number to set as the first number
Return Value: none
Algorithm:
Reference: none
********************************************************************* */
void Tile::setNum1(int setnum1)
{
	num1 = setnum1;
}

/* *********************************************************************
Function Name: getNum2
Purpose: To return the second number of the tile
Parameters:
	none
Return Value: num2
Algorithm:
Reference: none
********************************************************************* */
int Tile::getNum2()
{
	return num2;
}

/* *********************************************************************
Function Name: setNum2
Purpose: To set the second number of the tile
Parameters:
	setnum2, the number to set as the second number
Return Value: none
Algorithm:
Reference: none
********************************************************************* */
void Tile::setNum2(int setnum2)
{
	num2 = setnum2;
}

/* *********************************************************************
Function Name: getSum
Purpose: To return the sum of the two numbers on the tile
Parameters:
	none
Return Value: sum
Algorithm:
Reference: none
********************************************************************* */
int Tile::getSum()
{
	return sum;
}

/* *********************************************************************
Function Name: setSum
Purpose: To calculate and set the sum of the two numbers on the tile
Parameters:
	none
Return Value: none
Algorithm:
	1) Adds the two numbers of the tile
	2) Stores the result as the tile's sum
Reference: none
********************************************************************* */
void Tile::setSum()
{
	sum = num1 + num2;
}

/* *********************************************************************
Function Name: getDouble
Purpose: To return whether the tile is a double
Parameters:
	none
Return Value: isdouble
Algorithm:
Reference: none
********************************************************************* */
bool Tile::getDouble()
{
	return isdouble;
}

/* *********************************************************************
Function Name: checkDouble
Purpose: To check whether the tile is a double
Parameters:
	none
Return Value: none
Algorithm:
	1) Compares the two numbers of the tile
	2) Sets isdouble to true if the numbers are equal
	3) Sets isdouble to false if the numbers are different
Reference: none
********************************************************************* */
void Tile::checkDouble()
{
	if (num1 == num2)
	{
		isdouble = true;
	}
	else
	{
		isdouble = false;
	}
}

/* *********************************************************************
Function Name: getSpinner
Purpose: To return whether the tile is the spinner
Parameters:
	none
Return Value: isspinner
Algorithm:
Reference: none
********************************************************************* */
int Tile::getSpinner()
{
	return isspinner;
}

/* *********************************************************************
Function Name: setSpinner
Purpose: To set whether the tile is the spinner
Parameters:
	setisspinner, value indicating whether the tile is the spinner
Return Value: none
Algorithm:
Reference: none
********************************************************************* */
void Tile::setSpinner(bool setisspinner)
{
	isspinner = setisspinner;
}

/* *********************************************************************
Function Name: setTile
Purpose: To set both numbers of the tile
Parameters:
	setnum1, number of pips on one side of tile
	setnum2, number of pips on the other side of tile
Return Value: none
Algorithm:
	1) Sets the first and second numbers of the tile
	2) Calculates the sum of the two numbers
	3) Checks whether the tile is a double
Reference: none
********************************************************************* */
void Tile::setTile(int setnum1, int setnum2)
{
	num1 = setnum1;
	num2 = setnum2;
	setSum();
	checkDouble();
}

/* *********************************************************************
Function Name: swapNums
Purpose: To swap the two numbers of the tile (used for checking all info on tile)
Parameters:
	none
Return Value: none
Algorithm:
	1) Stores the first number in a temporary variable
	2) Sets the first number to the second number
	3) Sets the second number to the stored value
Reference: none
********************************************************************* */
void Tile::swapNums()
{
	int temp = num1;
	num1 = num2;
	num2 = temp;
}

/* *********************************************************************
Function Name: checkTile
Purpose: To check whether the tile matches the given numbers
Parameters:
	num1, first number to compare
	num2, second number to compare
Return Value: true if the tile matches the given numbers, otherwise false
Algorithm:
	1) Checks if the tile numbers match the given numbers in the same order
	1.1) Also checks if the tile numbers match the given numbers in reverse order
	3) Returns true if either order matches
	4) Returns false if neither order matches
Reference: none
********************************************************************* */
bool Tile::checkTile(int num1, int num2)
{
	if (this->num1 == num1 && this->num2 == num2 || this->num1 == num2 && this->num2 == num1)
	{
		return true;
	}
	else
	{
		return false;
	}
}

/* *********************************************************************
Function Name: printTile
Purpose: To print the numbers of the tile
Parameters:
	none
Return Value: none
Algorithm:
	1) Prints in format num1 then a dash then num2
Reference: none
********************************************************************* */
void Tile::printTile()
{
	cout << num1 << "-" << num2;
}

/* *********************************************************************
Function Name: newEnd
Purpose: To return the open end of the tile based on a given end
Parameters:
	end, board end to compare with the tile
Return Value: the other number of the tile if the given end matches, otherwise -1
Algorithm:
	1) Checks if the first number matches the given end
	1.1) If it matches, returns the second number
	2) Checks if the second number matches the given end
	2.1) If it matches, returns the first number
	3) Returns -1 if neither number matches the given end
Reference: none
********************************************************************* */
int Tile::newEnd(int end)
{
	if (num1 == end)
	{
		return num2;
	}
	else if (num2 == end)
	{
		return num1;
	}
	else
	{
		return -1;
	}
	
}

/* *********************************************************************
Function Name: writeTile
Purpose: To write the tile into a .txt file
Parameters:
	file, an ofstream attached to a .txt file
Return Value: none
Algorithm:
	1) Writes in format num1 then a dash then num2
Reference: none
********************************************************************* */
void Tile::writeTile(ofstream& file)
{
	file << " " << getNum1() << "-" << getNum2();
}

/* *********************************************************************
Function Name: readTile
Purpose: To read a tile from a .txt file
Parameters:
	in, an istream attached to the saved game data
Return Value: none
Algorithm:
	1) Reads the tile as a string from the file
	2) Finds the dash separating the two numbers
	3) Extracts the first and second numbers from the string
	4) Sets the tile numbers using the values read from the file
	5) Checks whether the tile is a double
Reference: none
********************************************************************* */
void Tile::readTile(istream& in)
{
	string tile;

	if (!(in >> tile))
	{
		return;
	}

	int dash = tile.find('-');

	if (dash == string::npos)
	{
		return;
	}

	num1 = stoi(tile.substr(0, dash));
	num2 = stoi(tile.substr(dash + 1));

	checkDouble();
}