/*
************************************************************
* Name:  Jared Novack
* Project : 35 Domino C++
* Class : OPL class 01
* Date : 9/20/2026
************************************************************
*/
#include "Board.h"
#include <cmath>
#include <vector>
#include <fstream>
#include <sstream>
#include <string>
#include <iostream>
using namespace std;

/* *********************************************************************
Function Name: getTarget
Purpose: To return the board target divisor
Parameters:
    none
Return Value: target
Algorithm:
Reference: none
********************************************************************* */
int Board::getTarget()
{
    return target;
}

/* *********************************************************************
Function Name: setTarget
Purpose: To set the target of board
Parameters:
    setTarget, input to change target value
Return Value: none
Algorithm:
Reference: none
********************************************************************* */
void Board::setSpinNum(int setspinnum)
{
    spinNum = setspinnum;
}

/* *********************************************************************
Function Name: getTarget
Purpose: To return the board target divisor
Parameters:
    none
Return Value: target
Algorithm:
Reference: none
********************************************************************* */
int Board::getSpinNum()
{
    return spinNum;
}

/* *********************************************************************
Function Name: setTarget
Purpose: To set the target of board
Parameters:
    setTarget, input to change target value
Return Value: none
Algorithm:
Reference: none
********************************************************************* */
void Board::setTarget(int settarget)
{
    target = settarget;
}

/* *********************************************************************
Function Name: getLastPlay
Purpose: To return the tile of the last play
Parameters:
    none
Return Value: lastPlay
Algorithm:
Reference: none
********************************************************************* */
Tile Board::getLastPlay()
{
    return lastPlay;
}

/* *********************************************************************
Function Name: getLastPlay
Purpose: To set the tile of the last play
Parameters:
    setLastPlay, input to change the last play
Return Value: none
Algorithm:
Reference: none
********************************************************************* */
void Board::setLastPlay(Tile setLastPlay)
{
    lastPlay = setLastPlay;
}

/* *********************************************************************
Function Name: getHorizontal
Purpose: To return the horizontal line of tiles on the board
Parameters:
  none
Return Value: horizontalLine
Algorithm:
Reference: none
********************************************************************* */
vector<Tile> Board::getHorizontal()
{
    return horizontalLine;
}

/* *********************************************************************
Function Name: getVertical
Purpose: To return the vertical line of tiles on the board
Parameters:
  none
Return Value: verticalLine
Algorithm:
Reference: none
********************************************************************* */
vector<Tile> Board::getVertical()
{
    return verticalLine;
}

/* *********************************************************************
Function Name: getHorSpinnerLoc
Purpose: To return the index of the spinner on the horizontal line
Parameters:
    none
Return Value: horSpinnerLoc
Algorithm:
Reference: none
********************************************************************* */
int Board::getHorSpinnerLoc()
{
    return horSpinnerLoc;
}

/* *********************************************************************
Function Name: getVerSpinnerLoc
Purpose: To return the index of the spinner on the vertical line
Parameters:
    none
Return Value: verSpinnerLoc
Algorithm:
Reference: none
********************************************************************* */
int Board::getVerSpinnerLoc()
{
    return verSpinnerLoc;
}

/* *********************************************************************
Function Name: getHasSpinner
Purpose: To return whether the board has a spinner
Parameters:
  none
Return Value: hasSpinner
Algorithm:
Reference: none
********************************************************************* */
bool Board::getHasSpinner()
{
    return hasSpinner;
}

/* *********************************************************************
Function Name: getPassCheck
Purpose: To return whether the last player has passed
Parameters:
  none
Return Value: passCheck
Algorithm:
Reference: none
********************************************************************* */
bool Board::getPassCheck()
{
    return passCheck;
}

/* *********************************************************************
Function Name: setPassCheck
Purpose: To set whether the current player has passed
Parameters:
  setPassCheck, input to change the pass check value
Return Value: none
Algorithm:
Reference: none
********************************************************************* */
void Board::setPassCheck(bool setPassCheck)
{
    passCheck = setPassCheck;
}

/* *********************************************************************
Function Name: addFirst
Purpose: To add the first tile to the horizontal board line and set the board ends
Parameters:
    tile, tile to add to the board
Return Value: none
Algorithm:
    1) If statement to check if tile is a double to set it as a spinner
Reference: none
********************************************************************* */
void Board::addFirst(Tile tile)
{
    if (tile.getDouble() && hasSpinner == false)
    {
        positionSpinner(tile);
        upEnd = tile.getNum1();
        downEnd = tile.getNum2();
    }
    else
    {
        leftEnd = tile.getNum1();
        rightEnd = tile.getNum2();
    }
    horizontalLine.push_back(tile);
    lastPlay = tile;
}

/* *********************************************************************
Function Name: addLeft
Purpose: To add a tile to the left side of the horizontal board line
Parameters:
    tile, tile to add to the left side of the board
Return Value: none
Algorithm:
    1) If and else if check if either side matches the pips of the left end to see if it can be added
    2) If statement to check if tile is a double and if no spinner is set to set tile as a spinner
Reference: none
********************************************************************* */
void Board::addLeft(Tile tile)
{
    if (tile.getNum2() == horizontalLine[0].getNum1())
    {
        horSpinnerLoc++;
        if (tile.getDouble() && hasSpinner == false)
        {
            positionSpinner(tile);
            horSpinnerLoc = 0;
            horizontalLine.insert(horizontalLine.begin(), tile);
            leftEnd = 0;
            lastPlay = tile;
        }
        else
        {
            horizontalLine.insert(horizontalLine.begin(), tile);
            leftEnd = tile.getNum1();
            lastPlay = tile;
        }
    }
    else if (tile.getNum1() == horizontalLine[0].getNum1())
    {
        horSpinnerLoc++;
        tile.swapNums();
        if (tile.getDouble() && hasSpinner == false)
        {
            positionSpinner(tile);
            horSpinnerLoc = 0;
            horizontalLine.insert(horizontalLine.begin(), tile);
            leftEnd = 0;
            lastPlay = tile;
        }
        else
        {
            horizontalLine.insert(horizontalLine.begin(), tile);
            leftEnd = tile.getNum1();
            lastPlay = tile;
        }
    }
}

/* *********************************************************************
Function Name: addRight
Purpose: To add a tile to the right side of the horizontal board line
Parameters:
    tile, tile to add to the right side of the board
Return Value: none
Algorithm:
    1) If and else if check if either side matches the pips of the right end to see if it can be added
    2) If statement to check if tile is a double and if no spinner is set to set tile as a spinner
Reference: none
********************************************************************* */
void Board::addRight(Tile tile)
{
    if (tile.getNum1() == horizontalLine[horizontalLine.size() - 1].getNum2())
    {
        if (tile.getDouble() && hasSpinner == false)
        {
            positionSpinner(tile);
            horSpinnerLoc = horizontalLine.size();
            horizontalLine.push_back(tile);
            rightEnd = 0;
            lastPlay = tile;
        }
        else
        {
            horizontalLine.push_back(tile);
            rightEnd = tile.getNum2();
            lastPlay = tile;
        }
    }
    else if (tile.getNum2() == horizontalLine[horizontalLine.size() - 1].getNum2())
    {
        tile.swapNums();
        if (tile.getDouble() && hasSpinner == false)
        {
            positionSpinner(tile);
            horSpinnerLoc = horizontalLine.size();
            horizontalLine.push_back(tile);
            rightEnd = 0;
            lastPlay = tile;
        }
        else
        {
            horizontalLine.push_back(tile);
            rightEnd = tile.getNum2();
            lastPlay = tile;
        }
    }
}

/* *********************************************************************
Function Name: addUp
Purpose: To add a tile to the top side of the vertical board line
Parameters:
    tile, tile to add to the top side of the board
Return Value: none
Algorithm:
    1) If and else if check if either side matches the pips of the up end to see if it can be added
Reference: none
********************************************************************* */
void Board::addUp(Tile tile)
{
    if (tile.getNum2() == verticalLine[0].getNum1())
    {
        verSpinnerLoc++;
        verticalLine.insert(verticalLine.begin(), tile);
        upEnd = tile.getNum1();
        lastPlay = tile;
    }
    else if (tile.getNum1() == verticalLine[0].getNum1())
    {
        verSpinnerLoc++;
        tile.swapNums();
        verticalLine.insert(verticalLine.begin(), tile);
        upEnd = tile.getNum1();
        lastPlay = tile;
    }
}

/* *********************************************************************
Function Name: addDown
Purpose: To add a tile to the bottom side of the vertical board line
Parameters:
    tile, tile to add to the bottom side of the board
Return Value: none
Algorithm:
    1) If and else if check if either side matches the pips of the down end to see if it can be added
Reference: none
********************************************************************* */
void Board::addDown(Tile tile)
{
    if (tile.getNum1() == verticalLine[verticalLine.size() - 1].getNum2())
    {
        verticalLine.push_back(tile);
        downEnd = tile.getNum2();
        lastPlay = tile;
    }
    else if (tile.getNum2() == verticalLine[verticalLine.size() - 1].getNum2())
    {
        tile.swapNums();
        verticalLine.push_back(tile);
        downEnd = tile.getNum2();
        lastPlay = tile;
    }
}

/* *********************************************************************
Function Name: positionSpinner
Purpose: To set a tile as the board spinner and initialize the vertical ends
Parameters:
    tile, tile to position as the spinner
Return Value: none
Algorithm:
Reference: none
********************************************************************* */
void Board::positionSpinner(Tile& tile)
{
    hasSpinner = true;
    tile.setSpinner(true);
    verSpinnerLoc = 0;
    upEnd = tile.getNum1();
    downEnd = tile.getNum2();
    spinNum = tile.getNum1();
    verticalLine.push_back(tile);
}

/* *********************************************************************
Function Name: getEnds
Purpose: To return the four current open ends of the board
Parameters:
    none
Return Value: ends/ vector containing the left, right, up, and down ends
Algorithm:
Reference: none
********************************************************************* */
vector<int> Board::getEnds()
{
    vector<int> ends;
    ends.push_back(leftEnd);
    ends.push_back(rightEnd);
    ends.push_back(upEnd);
    ends.push_back(downEnd);
    return ends;
}

/* *********************************************************************
Function Name: calcSum
Purpose: To calculate the sum of the current board ends and the value of a double tile
Parameters:
    tile, tile being evaluated
Return Value: sum/ calculated board sum
Algorithm:
    1) if the tile is a double add one of its end to the sum
Reference: none
********************************************************************* */
int Board::calcSum(Tile tile)
{
    int sum = leftEnd + rightEnd + upEnd + downEnd;
    if (tile.getDouble() == true && hasSpinner == true && tile.getSpinner() == false)
    {
        sum += tile.getNum1();
    }

    return sum;

}

/* *********************************************************************
Function Name: calcTurnScore
Purpose: To calculate the score after adding a tile to the board ends
Parameters:
    tile, tile being evaluated for the turn
Return Value: sum/ score if the sum is divisible by the board target, otherwise 0
Algorithm:
    1) returns not a 0 if the sum of ends is divisible by target cleanly
Reference: none
********************************************************************* */
int Board::calcTurnScore(Tile tile)
{
    int sum = calcSum(tile);
    if ((sum % target) == 0)
    {
        return sum;
    }
    else
    {
        return 0;
    }
}

/* *********************************************************************
Function Name: getAllMoves
Purpose: To determine all possible moves that can be made with the tiles in a hand
Parameters:
    hand, the hand of tiles to check for possible moves
Return Value: allMoves/ vector containing all possible moves and their scores
Algorithm:
    1) For loop to check each tile in the hand
    2) Check if the tile can be placed on the left, right, up, or down end of the board
    3) Calculate the score for each possible move
    4) Add each possible move and its information to the all moves vector
Reference: none
********************************************************************* */
vector<vector<int>> Board::getAllMoves(Hand hand)
{
    vector<vector<int>> allMoves;
    vector<int> tempscore;
    Tile temp;

    if (horizontalLine.size() == 0)
    {
        for (int i = 0; i < hand.getHandSize(); i++)
        {
            //cout << leftEnd << rightEnd << upEnd << downEnd;
            temp = hand.getHand()[i];
            int testL = temp.getNum1();
            int testR = temp.getNum2();

            int holdL = leftEnd;
            leftEnd = testL;

            int holdR = rightEnd;
            rightEnd = testR;

            allMoves.push_back({ temp.getNum1(), temp.getNum2(), -1, calcSum(temp), calcTurnScore(temp) });

            leftEnd = holdL;
            rightEnd = holdR;
        }
    }
    else
    {
        for (int i = 0; i < hand.getHandSize(); i++)
        {
            temp = hand.getHand()[i];
            int testL = temp.newEnd(horizontalLine[0].getNum1());
            int testR = temp.newEnd(horizontalLine[horizontalLine.size() - 1].getNum2());
            int testU = -1;
            int testD = -1;
            if (hasSpinner)
            {
                testU = temp.newEnd(verticalLine[0].getNum1());
                testD = temp.newEnd(verticalLine[verticalLine.size() - 1].getNum2());
            }
 
            //ZERO/0 EQUALS LEFT
            if (testL != -1)
            {
                int holdL = leftEnd;
                leftEnd = testL;
                allMoves.push_back({ temp.getNum1(), temp.getNum2(), 0, calcSum(temp), calcTurnScore(temp) });
                leftEnd = holdL;
            }

            //ONE/1 EQUALS RIGHT
            if (testR != -1)
            {
                int holdR = rightEnd;
                rightEnd = testR;
                allMoves.push_back({ temp.getNum1(), temp.getNum2(), 1, calcSum(temp), calcTurnScore(temp) });
                rightEnd = holdR;
            }

            //TWO/2 EQUALS UP
            if (testU != -1 && hasSpinner)
            {
                int holdU = upEnd;
                upEnd = testU;
                allMoves.push_back({ temp.getNum1(), temp.getNum2(), 2, calcSum(temp), calcTurnScore(temp) });
                upEnd = holdU;
            }

            //THREE/3 EQUALS DOWN
            if (testD != -1 && hasSpinner)
            {
                int holdD = downEnd;
                downEnd = testD;
                allMoves.push_back({ temp.getNum1(), temp.getNum2(), 3, calcSum(temp), calcTurnScore(temp) });
                downEnd = holdD;
            }
        }
    }

    return allMoves;
}

/* *********************************************************************
Function Name: optimalMoves
Purpose: To determine the moves with the highest possible score from the hand
Parameters:
    hand, the hand of tiles to check for possible moves
Return Value: optimalMoves/ vector containing the moves with the highest score
Algorithm:
    1) Calls getAllMoves to find all possible moves for the hand
    2) For loop to find the highest score among all possible moves
    3) For loop to add all moves with the highest score to the optimal moves vector
Reference: none
********************************************************************* */
vector<vector<int>> Board::optimalMoves(Hand hand)
{
    vector<vector<int>> allMoves = getAllMoves(hand);
    vector<vector<int>> optimalMoves;
    int max = -1;
    
    if (allMoves.size() != 0)
    {
        for (int i = 0; i < allMoves.size(); i++)
        {
            if (allMoves[i][4] > max)
            {
                max = allMoves[i][4];
            }
        }
    }

    if (max == -1)
    {
        return optimalMoves;
    }
    else
    {
        for (int i = 0; i < allMoves.size(); i++)
        {
            if (allMoves[i][4] == max)
            {
                optimalMoves.push_back(allMoves[i]);
            }
        }
    }

    return optimalMoves;
}

/* *********************************************************************
Function Name: removeFromHorizontal
Purpose: To remove and return the rightmost tile from the horizontal line
Parameters:
    none
Return Value: tempTile/ the removed tile
Algorithm:
Reference: none
********************************************************************* */
Tile Board::removeFromHorizontal()
{
    Tile tempTile;
    tempTile = horizontalLine[horizontalLine.size() - 1];
    horizontalLine.pop_back();
    return tempTile;
}
/* *********************************************************************
Function Name: removeFromVertical
Purpose: To remove and return the downmost tile from the vertical line
Parameters:
  none
Return Value: tempTile/ the removed tile
Algorithm:
Reference: none
********************************************************************* */
Tile Board::removeFromVertical()
{
    Tile tempTile;
    tempTile = verticalLine[verticalLine.size() - 1];
    verticalLine.pop_back();
    return tempTile;
}

/* *********************************************************************
Function Name: writeBoard
Purpose: To write the current board layout into a .txt file
Parameters:
    file, an ofstream attached to a .txt file
Return Value: none
Algorithm:
    1) Outputs the left side of the board layout
    2) For loop to output each tile in the horizontal line
    3) If statement to check for the spinner and output the vertical line
    4) Outputs the right side of the board layout
Reference: none
********************************************************************* */
void Board::writeBoard(ofstream& file)
{
    file << "Layout:   L";

    for (int i = 0; i < horizontalLine.size(); i++)
    {
        horizontalLine[i].printTile();
        cout << horizontalLine[i].getSpinner();
        if (horizontalLine[i].getSpinner())
        {
            file << " [D";

            for (int j = verticalLine.size() - 1; j >= 0; j--)
            {
                verticalLine[j].swapNums();
                verticalLine[j].writeTile(file);
            }

            file << " U]";
        }
        else
        {
            horizontalLine[i].writeTile(file);
        }
    }

    file << " R";
    file << endl;
}

/* *********************************************************************
Function Name: readBoard
Purpose: To read the board layout from a .txt file into the board vectors
Parameters:
    file, an ifstream attached to a .txt file
Return Value: none
Algorithm:
    1) Reads the horizontal tiles from the file until the spinner or right side is reached
    2) If a spinner is found, reads the vertical tiles and identifies the spinner
    3) Reads the remaining horizontal tiles after the vertical line
    4) Sets the board ends using the tiles that were read from the file
Reference: none
********************************************************************* */
void Board::readBoard(istream& file)
{
    string label;
    string leftLabel;
    string rightLabel;
    string spinnerLabel;
    string tileStr;

    file >> label;
    file >> leftLabel;   

    Tile tile;

    while (file)
    {
        file >> tileStr;

        if (tileStr == "[D")
        {
            break;
        }

        if (tileStr == "R")
        {
            break;
        }

        istringstream tileStream(tileStr);
        tile.readTile(tileStream);

        horizontalLine.push_back(tile);
    }

    // Read vertical tiles
    if (tileStr == "[D")
    {
        int spinnerValue = horizontalLine[horizontalLine.size() - 1].getNum2();

        while (file)
        {
            file >> tileStr;

            if (tileStr == "U]")
            {
                break;
            }

            istringstream tileStream(tileStr);
            tile.readTile(tileStream);

            if (tile.getDouble() && tile.getNum1() == spinnerValue && tile.getNum2() == spinnerValue)
            {
                tile.setSpinner(true);

                // Spinner belongs in both lines
                horizontalLine.push_back(tile);
                verticalLine.insert(verticalLine.begin(), tile);

                hasSpinner = true;
                horSpinnerLoc = horizontalLine.size() - 1;
                upEnd = tile.getNum1();
                downEnd = tile.getNum2();
            }
            else
            {
                tile.setSpinner(false);
                tile.swapNums();
                verticalLine.insert(verticalLine.begin(), tile);
            }
        }
    }

    for (int i = 0; i < verticalLine.size(); i++)
    {
        if (verticalLine[i].getSpinner())
        {
            verSpinnerLoc = i;
            break;
        }
    }

    //cout << "\nVERTICAL LINE:\n";

    //for (int i = 0; i < verticalLine.size(); i++)
    //{
    //    cout << i << ": "
    //        << verticalLine[i].getNum1()
    //        << "-"
    //        << verticalLine[i].getNum2()
    //        << " spinner = "
    //        << verticalLine[i].getSpinner()
    //        << endl;
    //}

    while (file)
    {
        file >> tileStr;

        if (tileStr == "R")
        {
            break;
        }

        istringstream tileStream(tileStr);
        tile.readTile(tileStream);
        tile.setSpinner(false);
        horizontalLine.push_back(tile);
    }

    if (horizontalLine.size() > 0)
    {
        leftEnd = horizontalLine[0].getNum1();
        rightEnd = horizontalLine[horizontalLine.size() - 1].getNum2();
    }

    if (verticalLine.size() > 0)
    {
        upEnd = verticalLine[0].getNum1();
        downEnd = verticalLine[verticalLine.size() - 1].getNum2();
    }
}