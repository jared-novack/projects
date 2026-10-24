#pragma once
#ifndef TOURNAMENT_H
#define TOURNAMENT_H

#include "Player.h"
#include "Human.h"
#include "Computer.h"

class Tournament
{
	private:
		int tScore;
	public:
		int getTScore();
		void setTScore(int setTScore);
		bool checkTornamentEnd(Human h, Computer c);
		void checkWinner(Human h, Computer c);
};

#endif
