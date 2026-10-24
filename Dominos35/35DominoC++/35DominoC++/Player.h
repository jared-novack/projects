#ifndef PLAYER_H
#define PLAYER_H

#include "Board.h"
#include "Deck.h"
#include "Hand.h"
#include <vector>
#include <fstream>
#include <string>
using namespace std;



class Player
{
private:
    Hand hand;
    int rPoints;
    int tPoints;

public:
    Hand& getHand();
    void setHand(Hand hand);
    int getRPoints();
    void addRPoints(int addrpoints);
    void resetRPoints();
    int getTPoints();
    void addTPoints(int addtPoints);
    void convertToTPoints();
    void writePlayer(ofstream& file);
    void readPlayer(istream& in);
    virtual Board playerTurn(Board board, Deck& deck) = 0;
};
#endif