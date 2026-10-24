#pragma once
#ifndef TILE_H
#define TILE_H

#include <vector>
#include <fstream>
#include <string>
using namespace std;



class Tile
{
private:
    int num1;
    int num2;
    int sum = -1;
    bool isdouble = false;
    bool isspinner = false;

public:
    int getNum1();
    void setNum1(int num1);
    int getNum2();
    void setNum2(int num2);
    int getSum();
    void setSum();
    bool getDouble();
    void checkDouble();
    int getSpinner();
    void setSpinner(bool isspinner);
    void setTile(int num1, int num2);
    void swapNums();
    bool checkTile(int num1, int num2);
    void printTile();
    int newEnd(int end);
    void writeTile(ofstream& file);
    void readTile(istream& in);

};
#endif