/*
************************************************************
* Name:  Jared Novack
* Project : 35 Domino C++
* Class : OPL class 01
* Date : 9/20/2026
************************************************************
*/
#include "Round.h"
#include <cmath>
#include <vector>
#include <string>
#include <sstream>
#include <iostream>
#include <fstream>
using namespace std;

/* *********************************************************************
Function Name: startGame
Purpose: To set up a new game or load a previously saved game
Parameters:
	none
Return Value: true if starting a new game, false if loading a saved game
Algorithm:
	1) Asks the user whether to start a new game or load a saved game
	2) If starting a new game, asks for the tournament ending score
	3) Sets the tournament score and creates a new deck
	4) If loading a game, asks for the saved file name
	5) Opens the file and loads the saved game data
	6) Repeats the input if an invalid choice or file is entered
Reference: none
********************************************************************* */
bool Round::startGame()
{
	char startCall = 'q';

	while (startCall != 'P' && startCall != 'L')
	{
		cout << "Play(P) or Load(L)? ";
		cin >> startCall;
		if (startCall != 'P' && startCall != 'L')
		{
			cout << "INVALID INPUT" << endl;
		}
	}

	int scoreCall = -1;

	while (scoreCall < 0 && startCall == 'P')
	{
		cout << "Enter valid Tournament Score to end Rounds at: ";
		cin >> scoreCall;
		if (scoreCall < 0)
		{
			cout << "INVALID INPUT" << endl;
		}
		else
		{
			tournament.setTScore(scoreCall);
			deck.newDeck();
		}
	}

	if (startCall == 'L')
	{
		string filename;
		cout << "Enter the filename to load (EX: game.txt): ";
		cin >> ws;
		getline(cin, filename);

		ifstream file(filename);
		if (!file.is_open())
		{
			cout << "Failed to open file: " << filename << endl;
			startGame();
		}
		else
		{
			loadGame(file);
			return false;
		}
	}

	return true;
}

/* *********************************************************************
Function Name: startRound
Purpose: To set up the beginning of a round (Doesnt run if a game is being loaded in)
Parameters:
	none
Return Value: none
Algorithm:
	1) Asks the user to select a target score of 3 or 5
	2) Sets the board target score
	3) Shuffles the deck
	4) Draws starting hands for the human and computer players
	5) Draws a tile for each player to determine who goes first
	5.1) Compares the tile sums and sets the current turn
	5.2) If the tile sums are equal, draws again
	5.3) Returns the drawn tiles to the deck
Reference: none
********************************************************************* */
void Round::startRound()
{
	int targetCall = -1;

	while (targetCall != 3 && targetCall != 5)
	{
		cout << "Enter Target Score of 3 or 5 for Round:  ";
		cin >> targetCall;
		if (targetCall != 3 && targetCall != 5)
		{
			cout << "INVALID INPUT" << endl;
		}
		else
		{
			board.setTarget(targetCall);
		}
	}

	deck.shuffleDeck();
	deck.startHandDraw(human.getHand());
	deck.startHandDraw(computer.getHand());
	//human.getHand().printHand();
	//computer.getHand().printHand();


	Tile tempH, tempC;
	currentTurn = -1;

	while (currentTurn == -1)
	{
		tempH = deck.drawFromDeck();
		tempC = deck.drawFromDeck();
		cout << "Drawing For Turn" << endl;
		cout << "Human Tile: [";
		tempH.printTile();
		cout << "]      Computer Tile: [";
		tempC.printTile();
		cout << "]" << endl;
		if (tempH.getSum() < tempC.getSum())
		{
			cout << "Human Player Sum: " << tempH.getSum() << "      Computer Player Sum: " << tempC.getSum() << endl;
			cout << "Computer Sum is larger and goes first!" << endl;
			currentTurn = 1;
		}
		else if (tempH.getSum() > tempC.getSum())
		{
			cout << "Human Player Sum: " << tempH.getSum() << "     Computer Player Sum: " << tempC.getSum() << endl;
			cout << "Human Sum is larger and goes first!" << endl;
			currentTurn = 0;
		}
		else
		{
			cout << "Human Player Sum: " << tempH.getSum() << "     Computer Player Sum: " << tempC.getSum() << endl;
			cout << "Human Sum and Computer Sum are the same! Draw again!" << endl;
		}
		deck.toBottomDeck(tempH);
		deck.toBottomDeck(tempC);
		cout << endl;
	}
}


/* *********************************************************************
Function Name: playingGame
Purpose: To run the main game play for the current round
Parameters:
	none
Return Value: true if the round ends normally, false if the game is suspended
Algorithm:
	1) Prints the initial board and number of tiles remaining in the deck
	2) While both players have tiles, checks which player's turn it is
	3) If it is the computer's turn, runs the computer's turn and updates its score
	4) If it is the human's turn, asks whether to continue playing or suspend the game
	4.1) If the human continues, runs the human's turn and updates the score
	4.2) If the human suspends the game, saves the current game and ends the function
	5) Switches the current turn after each completed turn
	6) Prints the updated board and number of tiles remaining in the deck
Reference: none
********************************************************************* */
bool Round::playingGame()
{

	char temp = 'q';
	cout << endl;
	viewer.printBoard(board);
	cout << endl;
	cout << "TILES IN DECK: " << deck.getDeckSize() << endl;
	cout << endl;

	while (human.getHand().getHandSize() != 0 && computer.getHand().getHandSize() != 0)
	{
		if (currentTurn == 1)
		{
			cout << "COMPUTER TURN: " << endl;
			if (board.getPassCheck())
			{
				board = computer.playerTurn(board, deck);

				currentTurn = 0;

				if (board.getPassCheck())
				{
					cout << "Multiple Turns in a row are passes. Round is over due to no more plays!" << endl << endl;
					break;
				}
				updateScore(computer);
			}
			else
			{
				board = computer.playerTurn(board, deck);
				updateScore(computer);

				currentTurn = 0;
			}

		}
		else
		{
			cout << "HUMAN TURN: " << endl;
			while (temp != 'P')
			{
				cout << "You want to continue playing turn (P) or suspend the game (S): " << endl;
				cin >> temp;
				if (temp == 'P')
				{
					board = human.playerTurn(board, deck);
					if (!board.getPassCheck())
					{
						updateScore(human);
					}
				}
				else if (temp == 'S')
				{
					saveGame();
					return false;
				}
				else
				{
					cout << "INVALID INPUT" << endl;
				}
			}
			if (temp == 'S')
			{
				break;
			}
			else
			{
				temp = 'q';
			}

			currentTurn = 1;

		}
		cout << endl;
		viewer.printBoard(board);
		cout << endl;
		cout << "TILES IN DECK: " << deck.getDeckSize() << endl;
		cout << endl;

	}
	return true;
}

/* *********************************************************************
Function Name: updateScore
Purpose: To calculate and add the current turn score to a player's round score
Parameters:
	player, player whose score will be updated
Return Value: none
Algorithm:
	1) Calculates the score from the most recent play
	2) Adds the score to the player's current round score
	3) Displays the points gained and the player's updated score
	4) Displays the current round scores for both players
Reference: none
********************************************************************* */
void Round::updateScore(Player& player)
{
	int score = board.calcTurnScore(board.getLastPlay());
	player.addRPoints(score);
	vector<int> ends = board.getEnds();

	cout << endl << "Left End pips: " << ends[0] << endl;
	cout << "Right End pips: " << ends[1] << endl;
	cout << "Up End pips: " << ends[2] << endl;
	cout << "Down End pips: " << ends[3] << endl << endl;

	if (board.getLastPlay().getDouble() == true && board.getLastPlay().getSpinner() == false)
	{
		cout << "Placed tile is a Non-Spinner Double. It is Counted Twice!" << endl << endl;
	}

	if (currentTurn == 0)
	{
		cout << "You gained " << score << " points. You have a total of " << player.getRPoints() << " points." << endl;
	}
	else
	{
		cout << "The computer gained " << score << " points. The computer has a total of " << player.getRPoints() << " points." << endl;
	}
	cout << endl;
	cout << "ROUND SCORE:" << endl;
	cout << "YOU: " << human.getRPoints() << " points" << endl;
	cout << "COMPUTER: " << computer.getRPoints() << " points" << endl << endl;
}

/* *********************************************************************
Function Name: endRound
Purpose: To calculate the round ending scores and determine whether the tournament continues
Parameters:
	none
Return Value: true if the tournament has ended, otherwise false
Algorithm:
	1) Calculates the remaining hand sums for both players
	2) Compares the hand sums to determine the round end bonus
	3) Adds the bonus points to the player with the smaller hand sum
	4) Displays the round scores
	5) Converts the round scores into tournament scores
	6) Checks whether either player has reached the tournament ending score
	7) If the tournament has ended, displays the winner
	8) Otherwise, restocks the deck and prepares for another round
Reference: none
********************************************************************* */
bool Round::endRound()
{
	human.getHand().updateHandSum();
	computer.getHand().updateHandSum();
	int hSum = human.getHand().getHandSum();
	int cSum = computer.getHand().getHandSum();
	int endBonus = 0;
	cout << "ROUND END:" << endl << endl;
	cout << "REMAINING HANDS:" << endl;
	cout << "YOU: ";
	human.getHand().printHand();
	cout << endl << "     Hand Sum: " << human.getHand().getHandSum() << endl;
	cout << "COMPUTER: ";
	computer.getHand().printHand();
	cout << endl << "     Hand Sum: " << computer.getHand().getHandSum() << endl;
	cout << endl;


	if (hSum < cSum)
	{
		cout << "The sum of pips in your hand is smaller than the computers! You get the round end bonus!"<< endl;
		cout << "Bonus calculation: " << cSum << " - " << hSum << " = " << (cSum - hSum) << endl;
		endBonus = calcEndScore((cSum - hSum));
		human.addRPoints(endBonus);
		cout << "You gain " << endBonus << " points." << "You have a total of " << human.getRPoints() << " points."<< endl;
		
	}
	else if (hSum > cSum)
	{
		cout << "The sum of pips in your hand is larger than the computers! The computer gets the round end bonus!" << endl;
		cout << "Bonus calculation: " << hSum << " - " << cSum << " = " << (hSum - cSum) << endl;
		endBonus = calcEndScore((hSum - cSum));
		computer.addRPoints(endBonus);
		cout << "The computer gained " << endBonus << " points. The computer has a total of " << computer.getRPoints() << " points." << endl;
	}
	else
	{
		cout << "The sum of pips in your hand is equal to the computers! No bonus awarded!" << endl;
	}

	cout << "ROUND END SCORES:" << endl;
	cout << "YOU: " << human.getRPoints() << endl;
	cout << "COMPUTER: " << computer.getRPoints() << endl;
	cout << endl;

	human.convertToTPoints();
	computer.convertToTPoints();

	cout << "PLAYER TOURNAMENT SCORES:" << endl;
	cout << "YOU: " << human.getTPoints() << endl;
	cout << "COMPUTER: " << computer.getTPoints() << endl;
	cout << endl;

	cout << "TOURNAMENT END SCORE: " << tournament.getTScore() << endl;
	cout << endl;
	

	if (tournament.checkTornamentEnd(human, computer))
	{
		cout << "THE TOURNAMENT IS OVER! ONE REACHED THE END SCORE!" << endl;
		cout << endl;
		tournament.checkWinner(human, computer);
		return true;
	}
	else
	{
		deck.restockDeck(human.getHand(), computer.getHand(), board);
		cout << "THE TOURNAMENT GOES ON!" << endl;
		cout << endl;
		return false;
	}
}

/* *********************************************************************
Function Name: calcEndScore
Purpose: To calculate the closest multiple of the round target score
Parameters:
	score, number of points used to calculate the ending score
Return Value: score, the closest multiple of the target represented as a quotient
Algorithm:
	1) Checks whether the score is evenly divisible by the target
	2) If it is divisible, divides the score by the target
	3) If it is not divisible, calculates the distance to the next lower and higher multiples
	4) Compares the distances to determine the closest multiple
	5) Returns the quotient of the closest multiple
Reference: none
********************************************************************* */
int Round::calcEndScore(int score)
{
	int temp = score;

	if(temp % board.getTarget() != 0)
	{
		int high = (temp / board.getTarget()) + 1;
		int low = (temp / board.getTarget());

		int highDistance = (high * board.getTarget()) - temp;
		int lowDistance = temp - (low * board.getTarget());

		if (highDistance < lowDistance)
		{
			temp = high;
			cout << "Closest multiple of " << board.getTarget() << " is " << (high * board.getTarget()) << endl;
			cout << (high * board.getTarget()) << " / " << board.getTarget() << " = ";
		}
		else
		{
			temp = low;
			cout << "Closest multiple of " << board.getTarget() << " is " << (low * board.getTarget()) << endl;
			cout << (low * board.getTarget()) << " / " << board.getTarget() << " = ";
		}
	}
	else
	{
		temp = score / board.getTarget();
		cout << "Closest multiple of " << board.getTarget() << " is " << score << endl;
		cout << score << " / " << board.getTarget() << " = ";
	}

	cout << temp << " points" << endl << endl;
	return temp;
}

/* *********************************************************************
Function Name: saveGame
Purpose: To save the current game state into a .txt file
Parameters:
	none
Return Value: none
Algorithm:
	1) Asks the user for a file name
	2) Opens the output file
	3) Writes the tournament score and target score
	4) Writes the computer player's hand and scores
	5) Writes the human player's hand and scores
	6) Writes the board layout and boneyard
	7) Writes the player whose turn is next
	8) Closes the file and displays a confirmation message
Reference: none
********************************************************************* */
void Round::saveGame()
{
	string fileName;

	cout << "Name the output '.txt' file (.txt added automatically on the end): ";
	cin >> fileName;
	fileName += ".txt";

	ofstream file(fileName);

	if (file.is_open())
	{
		file << "Tournament Score: " << tournament.getTScore();

		file << endl << endl;

		file << "Target Score: " << board.getTarget();

		file << endl << endl;

		file << "Computer:" << endl;
		computer.writePlayer(file);
		file << endl;

		file << "Human:" << endl;
		human.writePlayer(file);
		file << endl;

		board.writeBoard(file);
		file << endl;

		deck.writeDeck(file);
		file << endl;
		file << "Next Player: ";
		if (currentTurn == 1)
		{
			file << "Computer";
		}
		else
		{
			file << "Human";
		}

		file.close();

		cout << "GAME SAVED!" << endl;
	}
}


/* *********************************************************************
Function Name: loadGame
Purpose: To load a saved game state from a .txt file
Parameters:
	file, an ifstream containing the saved game data
Return Value: none
Algorithm:
	1) Reads the tournament score from the file
	2) Reads the target score for the round
	3) Reads the computer player's hand and scores
	4) Reads the human player's hand and scores
	5) Reads the board layout
	6) Reads the boneyard
	7) Reads which player has the next turn
	8) Sets the game variables using the values read from the file
Reference: none
********************************************************************* */
void Round::loadGame(ifstream& file)
{
	string line;

	getline(file, line);
	istringstream tournamentStream(line);

	string label1, label2;
	int tournamentScore;
	tournamentStream >> label1 >> label2 >> tournamentScore;
	tournament.setTScore(tournamentScore);

	getline(file, line);
	getline(file, line);
	istringstream targetStream(line);
	int targetScore;
	targetStream >> label1 >> label2 >> targetScore;
	board.setTarget(targetScore);

	getline(file, line);
	getline(file, line);
	computer.readPlayer(file);

	getline(file, line);
	getline(file, line);
	human.readPlayer(file);

	getline(file, line);
	getline(file, line);
	istringstream boardStream(line);
	board.readBoard(boardStream);

	getline(file, line);
	getline(file, line);
	istringstream deckStream(line);
	deck.readDeck(deckStream);

	getline(file, line);

	getline(file, line);

	istringstream nextStream(line);

	string turnStr;
	nextStream >> label1 >> label2 >> turnStr;

	if (turnStr == "Human")
	{
		currentTurn = 0;
	}
	else
	{
		currentTurn = 1;
	}
}

/* *********************************************************************
Function Name: checkEnd
Purpose: To check whether either player has no tiles remaining
Parameters:
	none
Return Value: true if either player has no tiles, otherwise false
Algorithm:
	1) Checks the number of tiles in the human player's hand and computer player's hand
	3) Returns false if both players still have tiles
	4) Returns true if either player has no tiles
Reference: none
********************************************************************* */
bool Round::checkEnd()
{
	if (human.getHand().getHandSize() != 0 && computer.getHand().getHandSize() != 0)
	{
		return false;
	}
	else
	{
		return true;
	}
}