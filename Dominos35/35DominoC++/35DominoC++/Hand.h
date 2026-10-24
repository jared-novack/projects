#ifndef HAND_H
#define HAND_H

#include "Tile.h"
#include <vector>
#include <fstream>
#include <string>
using namespace std;

class Hand
{
private:
    vector<Tile> playerHand;
    int handSum;

public:
    vector<Tile> getHand();
    int getHandSum();
    int getHandSize();
    bool isEmpty();
    void addToHand(Tile tile);
    bool hasTile(int num1, int num2);
    Tile removeFromHand();
    Tile removeFromHand(int num1, int num2);
    void updateHandSum();
    void printHand();
    void writeHand(ofstream& file);
    void readHand(istream& in);
};

#endif
