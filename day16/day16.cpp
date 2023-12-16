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

struct Tile {
    char type;
    int energized;
};

enum Direction {
    UP,
    DOWN,
    LEFT,
    RIGHT
};

struct Beam {
    int x;
    int y;
    Direction dir;
};

void printBoard(std::vector<std::vector<Tile>> & board)
{
    for (int i = 0; i < board[0].size(); i++)
    {
        for (int j = 0; j < board.size(); j++)
        {
            if (board[j][i].energized > 0)
            {
                printf("#");
            }
            else
            {
                printf("%c", board[j][i].type);
            }
        }
        printf("\n");
    }
}

void fireBeam(std::vector<std::vector<Tile>> & board, Beam & beam, std::unordered_map<std::string, bool> & m)
{
//    printf("Beam [%d, %d]\n", beam.x, beam.y);
//    printf("Tile [%c]\n", board[beam.x][beam.y].type);

    std::stringstream ss;
    ss << beam.x << "," << beam.y << "," << beam.dir;
    std::string key = ss.str();
    if (m[key])
        return;
    else
        m[key] = true;

    uint64_t maxIter = board.size() * board[0].size();
    uint64_t curIter = 0;

    while (beam.x < board.size() && beam.x >= 0 &&
            beam.y < board[0].size() && beam.y >= 0 &&
            curIter < maxIter)
    {
        board[beam.x][beam.y].energized += 1;
        if (board[beam.x][beam.y].type == '\\')
        {
            if (beam.dir == RIGHT)
                beam.dir = DOWN;
            else if (beam.dir == UP)
                beam.dir = LEFT;
            else if (beam.dir == LEFT)
                beam.dir = UP;
            else if (beam.dir == DOWN)
                beam.dir = RIGHT;
        }
        else if (board[beam.x][beam.y].type == '/')
        {
            if (beam.dir == RIGHT)
                beam.dir = UP;
            else if (beam.dir == UP)
                beam.dir = RIGHT;
            else if (beam.dir == LEFT)
                beam.dir = DOWN;
            else if (beam.dir == DOWN)
                beam.dir = LEFT;
        }
        else if (board[beam.x][beam.y].type == '|')
        {
            if (beam.dir == RIGHT || beam.dir == LEFT)
            {
                beam.dir = DOWN;
                Beam newBeam = {beam.x, beam.y, beam.dir};
                newBeam.dir = UP;
                fireBeam(board, newBeam, m);
            }
        }
        else if (board[beam.x][beam.y].type == '-')
        {
            if (beam.dir == UP || beam.dir == DOWN)
            {
                beam.dir = LEFT;
                Beam newBeam = {beam.x, beam.y, beam.dir};
                newBeam.dir = RIGHT;
                fireBeam(board, newBeam, m);
            }
        }

        switch(beam.dir)
        {
            case UP:
                beam.y--;
                break;
            case DOWN:
                beam.y++;
                break;
            case LEFT:
                beam.x--;
                break;
            case RIGHT:
                beam.x++;
                break;
        }
        curIter++;
    }
}

uint64_t countEnergizedTiles(std::vector<std::vector<Tile>> & board)
{
    int energizedTiles = 0;
    for (int j = 0; j < board[0].size(); j++)
    {
        for (int i = 0; i < board.size(); i++)
        {
            if (board[i][j].energized > 0)
                energizedTiles++;
        }
    }
    return energizedTiles;
}

void clearTiles(std::vector<std::vector<Tile>> & board)
{
    for (int j = 0; j < board[0].size(); j++)
    {
        for (int i = 0; i < board.size(); i++)
        {
            board[i][j].energized = 0;
        }
    }
}

int main()
{
    FILE * f = fopen("day16/day16.txt", "r");

    // ---------------------------------------------------------
    // our data structures
    std::vector<std::string> lines;

    // ---------------------------------------------------------
    // Parse input into data structures
    char * line = nullptr;
    size_t len;
    while (getline(&line, &len, f) != -1)
    {
        std::string strLine = std::string(line);
        strLine = std::regex_replace(strLine, std::regex("\n"), "");
        std::cout << strLine << std::endl;
        lines.push_back(strLine);
    }
    free(line);

    int boardSizeX = lines.at(0).length();
    int boardSizeY = lines.size();

    std::vector<std::vector<Tile>> board(boardSizeX, std::vector<Tile>(boardSizeY, {'.', 0}));

    for (int j = 0; j < boardSizeY; j++)
    {
        for (int i = 0; i < boardSizeX; i++)
        {
            Tile t = {lines.at(j).at(i), 0};
            board[i][j] = t;
        }
    }


    // ---------------------------------------------------------
    // Part 1
    Beam b = {0,0, RIGHT};
    printf("[%d, %d]\n", b.x, b.y);
    std::unordered_map<std::string, bool> beamDirs;
//    b.loc += b.dir;
//    printf("[%d, %d]\n", b.loc.x, b.loc.y);

    printBoard(board);
    fireBeam(board, b, beamDirs);
    printBoard(board);

    int energizedTiles = 0;
    for (int j = 0; j < boardSizeY; j++)
    {
        for (int i = 0; i < boardSizeX; i++)
        {
            if (board[i][j].energized > 0)
                energizedTiles++;
        }
    }
    printf("Part 1: %d\n", energizedTiles);

    // ---------------------------------------------------------
    // Part 2
    uint64_t maxEnergizedTiles = 0;
    for (int i = 0; i < boardSizeX; i++)
    {
        if (i == 0 || i == boardSizeX) {
            for (int j = 0; j < boardSizeY; j++)
            {
                if (i == 0)
                    b = {i,j, RIGHT};
                else
                    b = {i,j, LEFT};
                beamDirs.clear();
                clearTiles(board);
                fireBeam(board, b, beamDirs);
                if (countEnergizedTiles(board) > maxEnergizedTiles)
                    maxEnergizedTiles = countEnergizedTiles(board);
            }
        }
        for (int j = 0; j < boardSizeY; j += boardSizeY-1)
        {
            if (j == 0)
                b = {i,j, DOWN};
            else
                b = {i,j, UP};
            beamDirs.clear();
            clearTiles(board);
            fireBeam(board, b, beamDirs);
            if (countEnergizedTiles(board) > maxEnergizedTiles)
                maxEnergizedTiles = countEnergizedTiles(board);
        }
    }
    printf("Part 2: %lu\n", maxEnergizedTiles);

    return 0;
}
