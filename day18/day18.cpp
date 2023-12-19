#include <cmath>
#include <iostream>
#include <numeric>
#include <regex>
#include <variant>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <inttypes.h>

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
    int x;
    int y;
    std::string color;
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
    std::string color;
};

bool isInLoop(std::vector<std::vector<Hole>> & board, int boardSizeX, int boardSizeY, int x, int y)
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
    Tile startingTile = {0, 0, ""};
    cornerTiles.push_back(startingTile);
    int minX = 0;
    int maxX = 0;
    int minY = 0;
    int maxY = 0;
    for (auto l : lines)
    {
        Tile lastTile = cornerTiles.back();
        Tile t = {lastTile.x, lastTile.y, ""};
        auto tokens = split(l, " ");
        // location
        if (tokens.at(0) == "U")
            t.y = lastTile.y - std::stoi(tokens.at(1));
        else if (tokens.at(0) == "D")
            t.y = lastTile.y + std::stoi(tokens.at(1));
        else if (tokens.at(0) == "L")
            t.x = lastTile.x - std::stoi(tokens.at(1));
        else if (tokens.at(0) == "R")
            t.x = lastTile.x + std::stoi(tokens.at(1));
        // color
        t.color = tokens.at(2);
        // push
        cornerTiles.push_back(t);

        // get new min/max x/y
        if (t.x < minX) minX = t.x;
        if (t.x > maxX) maxX = t.x;
        if (t.y < minY) minY = t.y;
        if (t.y > maxY) maxY = t.y;
    }
    // fix coordinates
    for (auto & t : cornerTiles)
    {
        printf("[%d, %d] %s\n", t.x, t.y, t.color.c_str());
        if (minX < 0)
            t.x -= minX;
        if (minY < 0)
            t.y -= minY;
    }

    // Init board
    int boardSizeX = maxX - minX + 1;
    int boardSizeY = maxY - minY + 1;
    //Hole board[boardSizeX][boardSizeY];
    std::vector<std::vector<Hole>> board(boardSizeX, std::vector<Hole>(boardSizeY));
    for (int j = 0; j < boardSizeY; j++)
    {
        for (int i = 0; i < boardSizeX; i++)
        {
            board[i][j] = {NOT, ""};
        }
    }

    // set corners and edges
    for (int i = 0; i < cornerTiles.size(); i++)
    {
//        HoleType ht;
//        if ( cornerTiles.at(i + cornerTiles.size() - 1 % cornerTiles.size()).y < cornerTiles.at(i).y ||
//             cornerTiles.at(i + cornerTiles.size() + 1 % cornerTiles.size()).y < cornerTiles.at(i).y)
//            ht = CORNER_DOWN;
//        else
//            ht = CORNER_UP;
        board[cornerTiles.at(i).x][cornerTiles.at(i).y] = {CORNER, cornerTiles.at(i).color};

        // fill edge
        Tile curTile = cornerTiles.at(i);
        Tile nextTile = (i == cornerTiles.size() - 1 ? cornerTiles.at(0) : cornerTiles.at(i+1));
        int xstep = curTile.x - nextTile.x <= 0 ? (curTile.x - nextTile.x == 0 ? 0 : 1) : -1;
        int ystep = curTile.y - nextTile.y <= 0 ? (curTile.y - nextTile.y == 0 ? 0 : 1) : -1;
        int x = curTile.x + xstep;
        int y = curTile.y + ystep;
        while (x != nextTile.x || y != nextTile.y)
        {
            board[x][y] = {EDGE, ""};
            x += xstep;
            y += ystep;
        }
    }

    // fill in board
    for (int i = 0; i < boardSizeX; i++)
    {
        for (int j = 0; j < boardSizeY; j++)
        {
            if (board[i][j].type != NOT)
                continue;
            if (isInLoop(board, boardSizeX, boardSizeY, i, j))
                board[i][j].type = FILL;
        }
    }

    // print board
    for (int j = 0; j < boardSizeY; j++)
    {
        for (int i = 0; i < boardSizeX; i++)
        {
            if (board[i][j].type == EDGE || board[i][j].type == CORNER)
                printf("#");
            else if (board[i][j].type == FILL)
                printf("@");
            else printf(".");
        }
        printf("\n");
    }

    // count holes
    uint64_t holes = 0;
    for (int j = 0; j < boardSizeY; j++)
    {
        for (int i = 0; i < boardSizeX; i++)
        {
            if (board[i][j].type == EDGE || board[i][j].type == CORNER || board[i][j].type == FILL)
                holes++;
        }
    }
    printf("Part 1: %lu\n", holes);

    return 0;
}
