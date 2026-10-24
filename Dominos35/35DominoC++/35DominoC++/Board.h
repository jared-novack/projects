#ifndef BOARD_H
#define BOARD_H

#include "Tile.h"
#include "Hand.h"
#include <vector>
#include <fstream>
using namespace std;

class Board
{
private:
    vector<Tile> horizontalLine;
    vector<Tile> verticalLine;
    int target;
    int leftEnd = 0;
    int rightEnd = 0;
    int upEnd = 0;
    int downEnd = 0;
    int spinNum = -1;
    int horSpinnerLoc;
    int verSpinnerLoc;
    bool hasSpinner = false;
    bool passCheck = false;
    Tile lastPlay;

public:
    int getTarget();
    void setTarget(int settarget);
    int getSpinNum();
    void setSpinNum(int setspinnum);
    Tile getLastPlay();
    void setLastPlay(Tile setLastPlay);
    vector<Tile> getHorizontal();
    vector<Tile> getVertical();
    int getHorSpinnerLoc();
    int getVerSpinnerLoc();
    bool getHasSpinner();
    bool getPassCheck();
    void setPassCheck(bool setPassCheck);
    void addFirst(Tile tile);
    void addLeft(Tile tile);
    void addRight(Tile tile);
    void addUp(Tile tile);
    void addDown(Tile tile);
    void positionSpinner(Tile& tile);
    vector<int> getEnds();
    int calcSum(Tile tile);
    int calcTurnScore(Tile tile);
    vector<vector<int>> getAllMoves(Hand hand);
    vector<vector<int>> optimalMoves(Hand hand);
    Tile removeFromHorizontal();
    Tile removeFromVertical();
    void writeBoard(ofstream& file);
    void readBoard(istream& file);
};

#endif
