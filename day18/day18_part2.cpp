#include <cmath>
#include <iostream>
#include <numeric>
#include <regex>
#include <variant>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <inttypes.h>
#include <math.h>

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

enum Direction
{
    UP,
    DOWN,
    LEFT,
    RIGHT,
    NONE
};

struct Instruction
{
    Direction dir;
    int length;
    std::string color;
};

struct Tile
{
    int64_t x;
    int64_t y;
};

enum HoleType
{
    CORNER,
    EDGE,
    FILL,
    NOT
};

struct Hole
{
    HoleType type;
};

bool isInLoop(std::vector<std::vector<Hole>> & board, int64_t boardSizeX, int64_t boardSizeY, int64_t x, int64_t y)
{
    int crosses = 0;
    //printf("[%d, %d] {%d, %d}\n", x, y, boardSizeX, boardSizeY);
    //printf("%d -> ", board[x][y].type);
    while(++x < boardSizeX)
    {
        //printf("%d -> ", board[x][y].type);
        if (y < boardSizeY - 1 && y > 0 && board[x][y].type == EDGE &&
            (board[x][y+1].type == EDGE || board[x][y+1].type == CORNER) &&
            (board[x][y-1].type == EDGE || board[x][y-1].type == CORNER) )
        {
            //printf("(edge)");
            crosses++;
        }
        else if (board[x][y].type == CORNER)
        {
            // test if down or up corner, only count down corners
            if (y < boardSizeY - 1 && board[x][y+1].type == EDGE)
            {
                //printf("(down edge)");
                crosses++;
            }
        }
    }
    //printf(" %d\n", crosses);
    if (crosses % 2 == 1)
        return true;
    else
        return false;
}

int main()
{
    FILE * f = fopen("day18/day18.txt", "r");

    // ---------------------------------------------------------
    // our data structures
    std::vector<std::string> lines;

    // ---------------------------------------------------------
    // Parse input into data structures
    char * line = nullptr;
    size_t len;

    bool first_line = true;
    uint32_t size = 0;
    while (getline(&line, &len, f) != -1)
    {
        std::string strLine = std::string(line);
        strLine = std::regex_replace(strLine, std::regex("\n"), "");
        std::cout << strLine << std::endl;

        lines.push_back(strLine);
    }
    free(line);

    // create vector of corner locations
    // keep track of min/max x/y
    std::vector<Tile> cornerTiles;
    Tile startingTile = {0, 0};
    Tile lastTile = {0, 0};
    //cornerTiles.push_back(startingTile);
    int64_t minX = 0;
    int64_t maxX = 0;
    int64_t minY = 0;
    int64_t maxY = 0;
    for (auto l : lines)
    {
        Tile t = {lastTile.x, lastTile.y};
        auto tokens = split(l, " ");
        // location
        if (tokens.at(2).substr(7, 1) == "3")
            t.y = lastTile.y - std::stol("0x" + tokens.at(2).substr(2, 5), nullptr, 16);
        else if (tokens.at(2).substr(7, 1) == "1")
            t.y = lastTile.y + std::stol("0x" + tokens.at(2).substr(2, 5), nullptr, 16);
        else if (tokens.at(2).substr(7, 1) == "2")
            t.x = lastTile.x - std::stol("0x" + tokens.at(2).substr(2, 5), nullptr, 16);
        else if (tokens.at(2).substr(7, 1) == "0")
            t.x = lastTile.x + std::stol("0x" + tokens.at(2).substr(2, 5), nullptr, 16);
//        if (tokens.at(0) == "U")
//            t.y = lastTile.y - std::stoi(tokens.at(1));
//        else if (tokens.at(0) == "D")
//            t.y = lastTile.y + std::stoi(tokens.at(1));
//        else if (tokens.at(0) == "L")
//            t.x = lastTile.x - std::stoi(tokens.at(1));
//        else if (tokens.at(0) == "R")
//            t.x = lastTile.x + std::stoi(tokens.at(1));

        // push
        cornerTiles.push_back(t);

        // get new min/max x/y
        if (t.x < minX) minX = t.x;
        if (t.x > maxX) maxX = t.x;
        if (t.y < minY) minY = t.y;
        if (t.y > maxY) maxY = t.y;

        lastTile = cornerTiles.back();
    }
    // fix coordinates
    for (auto & t : cornerTiles)
    {
        printf("[%lu, %lu]\n", t.x, t.y);
        if (minX < 0)
            t.x -= minX;
        if (minY < 0)
            t.y -= minY;
    }

    // Init board
    int64_t boardSizeX = maxX - minX + 1;
    int64_t boardSizeY = maxY - minY + 1;

    int64_t area = 0;
    for (int64_t i = 0; i < cornerTiles.size(); i++)
    {
        Tile curTile = cornerTiles.at(i);
        Tile nextTile = (i == cornerTiles.size() - 1 ? cornerTiles.at(0) : cornerTiles.at(i+1));
        Tile prevTile = (i == 0 ? cornerTiles.at(cornerTiles.size()-1) : cornerTiles.at(i-1));

        area += curTile.y * (prevTile.x - nextTile.x) + abs(curTile.x - nextTile.x) + abs(curTile.y - nextTile.y);
    }
    area = (area >> 1) + 1;

    printf("Part 2: %ld\n", area);

    return 0;
}
