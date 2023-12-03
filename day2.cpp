#include <iostream>
#include <regex>
#include <variant>
#include <sstream>
#include <unordered_map>
#include <vector>

#include "inttypes.h"

void trim(std::string & str)
{
    size_t first = str.find_first_not_of(' ');
    size_t last = str.find_last_not_of(' ');
    if (first == std::variant_npos)
        first = 0;
    if (last == std::variant_npos)
        last = str.length();

    str = str.substr(first, last - first + 1);
}

std::vector<std::string> split(std::string & input, const char * delim)
{
    std::vector<std::string> tokens;
    std::string remainingString = input;
    size_t loc;
    while ( (loc = remainingString.find_first_of(delim)) != std::variant_npos )
    {
        tokens.push_back(remainingString.substr(0, loc));
        remainingString = remainingString.substr(loc+1);
    }
    tokens.push_back(remainingString);

    return tokens;
}

struct Game
{
    int ID;
    std::vector<std::unordered_map<std::string, int>> ballDraws;
};

int main()
{
    FILE * f = fopen("day2/day2.txt", "r");

    // std::string test("tqqqqqasdasdas  sads dsa s   asdasdasdas yyuyuyr    ");
    // trim(test);
    // printf(":%s:\n", test.c_str());
    // return 1;

    std::vector<Game> games;

    char * line = nullptr;
    size_t len;

    uint32_t size = 0;
    while (getline(&line, &len, f) != -1)
    {
        std::string strLine = std::string(line);
        strLine = std::regex_replace(strLine, std::regex("\n"), "");
        std::cout << strLine << std::endl;

        size_t colon;
        if ( (colon = strLine.find_first_of(':')) != std::variant_npos)
        {
            // the game ID
            int gameID;
            sscanf(strLine.c_str(), "Game %d:", &gameID);
            Game curGame;
            curGame.ID = gameID;

            // Split into draws
            std::string draws = strLine.substr(colon+1);
            trim(draws);
            auto eachDraws = split(draws, ";");
            for (auto d : eachDraws)
            {
                trim(d);
                printf("Draw: %s\n", d.c_str());

                // split into each ball count
                auto ballCounts = split(d, ",");
                std::unordered_map<std::string, int> ballPairs;
                for (auto bc : ballCounts)
                {
                    trim(bc);
                    auto countColourPair = split(bc, " ");
                    std::cout << "Count: " << countColourPair[0] << " Colour: " << countColourPair[1] << std::endl;
                    ballPairs[countColourPair[1]] = std::stoi(countColourPair[0]);
                }
                curGame.ballDraws.push_back(ballPairs);
            }
            games.push_back(curGame);
        }
    }
    free(line);


    int sumOfPossibleIDs = 0;
    int sumOfPower = 0;
    for (auto game : games)
    {
        printf("Game: %d has %d draws\n", game.ID, game.ballDraws.size());
        bool isImpossible = false;
        int maxRed = 0;
        int maxBlue = 0;
        int maxGreen = 0;
        for (auto ballDraw : game.ballDraws)
        {
            maxRed = ballDraw["red"] > maxRed ? ballDraw["red"] : maxRed;
            maxBlue = ballDraw["blue"] > maxBlue ? ballDraw["blue"] : maxBlue;
            maxGreen = ballDraw["green"] > maxGreen ? ballDraw["green"] : maxGreen;

            if (ballDraw["red"] > 12 || ballDraw["green"] > 13 || ballDraw["blue"] > 14)
            {
                isImpossible = true;
                continue;
            }
        }
        if (!isImpossible)
            sumOfPossibleIDs += game.ID;
        sumOfPower += (maxRed * maxGreen * maxBlue);
    }

    printf("Sum of possible IDS: %d\n", sumOfPossibleIDs);
    printf("Sum of powers: %d\n", sumOfPower);

    return 0;
}
