#pragma once
#ifndef COMPUTER_H
#define COMPUTER_H
#include "Player.h"
#include "Board.h"
#include <vector>
using namespace std;


class Computer : public Player
{
private:

public:
	Board playerTurn(Board board, Deck& deck) override;
};

#endif