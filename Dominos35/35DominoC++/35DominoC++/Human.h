#pragma once
#ifndef HUMAN_H
#define HUMAN_H
#include "Player.h"
#include "Board.h"
#include <vector>
using namespace std;


class Human : public Player
{
private:

public:	
	Board playerTurn(Board board, Deck& deck) override;
};

#endif