#pragma once
#pragma once
#ifndef ROUND_H
#define ROUND_H
#include "Player.h"
#include "Human.h"
#include "Computer.h"
#include "Deck.h"
#include "Board.h"
#include "BoardView.h"
#include "Tournament.h"
#include <vector>
using namespace std;


class Round
{
private:
	Human human;
	Computer computer;
	Board board;
	BoardView viewer;
	Deck deck;
	Tournament tournament;
	int currentTurn;
public:
	bool startGame();
	void startRound();
	bool playingGame();
	void updateScore(Player& player);
	bool endRound();
	int calcEndScore(int score);
	void saveGame();
	void loadGame(ifstream& file);
	bool checkEnd();

};

#endif