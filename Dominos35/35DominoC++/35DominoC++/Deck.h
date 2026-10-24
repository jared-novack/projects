#ifndef DECK_H
#define DECK_H

#include "Tile.h"
#include "Hand.h"
#include "Board.h"
#include <vector>
#include <fstream>
#include <string>
using namespace std;

class Deck
{
private:
    vector<Tile> deck;

public:
    vector<Tile> getDeck();
    void addToDeck(Tile tile);
    int getDeckSize();
    bool isEmpty();
    void toBottomDeck(Tile tile);
    Tile drawFromDeck();
    Tile removeFromDeck(int num1, int num2);
    void printDeck();
    void shuffleDeck();
    void startHandDraw(Hand& hand);
    void restockDeck(Hand& h1, Hand& h2, Board& b);
    void newDeck();
    void writeDeck(ofstream& file);
    void readDeck(istream& in);
};

#endif
