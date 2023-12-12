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

struct SLoc
{
    int x;
    int y;
};

std::vector<int> parseNumbers(std::string & numbers)
{
    std::vector<int> parsedNumbers;

    trim(numbers);
    // split into each ball count
    auto nums = split(numbers, " ");
    for (auto num : nums)
    {
        trim(num);
        if (num.empty())
            continue;
        parsedNumbers.push_back(std::stoi(num));
    }
    return parsedNumbers;
}



int main()
{
    FILE * f = fopen("day10/day10.txt", "r");

    // ---------------------------------------------------------
    // our data structures
    std::vector<std::string> lines;
    SLoc sLocation = {0, 0};

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

    char board[lines.at(0).size()][lines.size()];
    int boardXSize = lines.at(0).length();
    int boardYSize = lines.size();
    printf("Board: [%d, %d]\n", boardXSize, boardYSize);

    for (int i = 0; i < lines.size(); i++)
    {
        for (int j = 0; j < lines.at(0).length(); j++)
        {
            board[j][i] = lines.at(i).at(j);
            printf("%c", board[j][i]);

            if (board[j][i] == 'S')
            {
                sLocation.x = j;
                sLocation.y = i;
            }
        }
        printf("\n");
    }

    // replace S on the board
    // this is hardcoded from looking at the input
    // family visiting and did not have time
    board[sLocation.x][sLocation.y] = 'J';


    std::vector<SLoc> loop;
    loop.push_back(sLocation);
    SLoc sLocStart = {sLocation.x, sLocation.y};

    Direction lastMove = Direction::NONE;

    int loopSize = 0;
    // traverse until finding start
    do
    {
        printf("S [%d, %d]\n", sLocation.x, sLocation.y);
        // Look in cardinal directions for options
        // UP
        if ( lastMove != DOWN && sLocation.y > 0 &&
            (board[sLocation.x][sLocation.y] == '|' || board[sLocation.x][sLocation.y] == 'J' || board[sLocation.x][sLocation.y] == 'L' || board[sLocation.x][sLocation.y] == 'S') &&
            (board[sLocation.x][sLocation.y-1] == '|' || board[sLocation.x][sLocation.y-1] == 'F' || board[sLocation.x][sLocation.y-1] == '7')
            )
        {
            sLocation.y--;
            lastMove = UP;
            //printf("Up\n");
        }
        // DOWN
        else if ( lastMove != UP && sLocation.y < boardYSize-1 &&
            (board[sLocation.x][sLocation.y] == '|' || board[sLocation.x][sLocation.y] == 'F' || board[sLocation.x][sLocation.y] == '7' || board[sLocation.x][sLocation.y] == 'S') &&
            (board[sLocation.x][sLocation.y+1] == '|' ||
            board[sLocation.x][sLocation.y+1] == 'L' ||
            board[sLocation.x][sLocation.y+1] == 'J')
            )
        {
            sLocation.y++;
            lastMove = DOWN;
            //printf("Down\n");
        }
        // LEFT
        else if ( lastMove != RIGHT && sLocation.x > 0 &&
            (board[sLocation.x][sLocation.y] == '-' || board[sLocation.x][sLocation.y] == 'J' || board[sLocation.x][sLocation.y] == '7' || board[sLocation.x][sLocation.y] == 'S') &&
            (board[sLocation.x-1][sLocation.y] == 'L' || board[sLocation.x-1][sLocation.y] == 'F' || board[sLocation.x-1][sLocation.y] == '-')
            )
        {
            sLocation.x--;
            lastMove = LEFT;
            //printf("Left\n");
        }
        // RIGHT
        else if ( lastMove != LEFT && sLocation.x < boardXSize-1 &&
            (board[sLocation.x][sLocation.y] == '-' || board[sLocation.x][sLocation.y] == 'F' || board[sLocation.x][sLocation.y] == 'L' || board[sLocation.x][sLocation.y] == 'S') &&
            (board[sLocation.x+1][sLocation.y] == '-' || board[sLocation.x+1][sLocation.y] == 'J' || board[sLocation.x+1][sLocation.y] == '7')
            )
        {
            sLocation.x++;
            lastMove = RIGHT;
            //printf("Right\n");
        }
        else
        {
            break;
        }
        loopSize++;

        loop.push_back(sLocation);
        //printf("S [%d, %d]\n", sLocation.x, sLocation.y);
    } while(sLocation.x != sLocStart.x || sLocation.y != sLocStart.y);
    loopSize++;

    printf("Loop size: %d\n", loopSize);

    printf("Part 1: %d\n", loopSize / 2);

    // Part 2
    // for every square, count how many times it crosses the loop
    // odd number of crosses means that it is enclosed
    int enclosedSquares = 0;
    for (int i = 1; i < boardXSize-1; i++)
    {
        for (int j = 1; j < boardYSize-1; j++)
        {
            bool isOnLoop = false;
            for (auto loc : loop)
            {
                if (loc.x == i && loc.y == j)
                {
                    isOnLoop = true;
                    break;
                }
            }

            if (!isOnLoop)
            {
                int newX = i;
                int loopCrosses = 0;
                char gotOnLoopDir = NONE;
                // go left until 0
                while (newX > 0)
                {
                    newX--;
                    bool isNewXOnLoop = false;
                    for (auto loc : loop)
                    {
                        if (loc.x == newX && loc.y == j)
                        {
                            isNewXOnLoop = true;
                            break;
                        }
                    }

                    if (isNewXOnLoop)
                    {
                        // check if we are crossing a loop boundry
                        if ( board[newX][j] == '|' )
                        {
                            loopCrosses++;
                            gotOnLoopDir = NONE;
                        }
                        else if ( gotOnLoopDir == NONE && board[newX][j] == 'J' )
                        {
                            loopCrosses++;
                            gotOnLoopDir = DOWN;
                        }
                        else if ( gotOnLoopDir == NONE && board[newX][j] == '7' )
                        {
                            loopCrosses++;
                            gotOnLoopDir = UP;
                        }
                        else if ( board[newX][j] == 'L' )
                        {
                            if (gotOnLoopDir == DOWN)
                            {
                                loopCrosses++;
                            }
                            gotOnLoopDir = NONE;
                        }
                        else if ( board[newX][j] == 'F' )
                        {
                            if (gotOnLoopDir == UP)
                            {
                                loopCrosses++;
                            }
                            gotOnLoopDir = NONE;
                        }
                    }
                }
                if (loopCrosses % 2 == 1)
                {
                    enclosedSquares++;
                    printf("Enclosed: [%d, %d]\n", i, j);
                }
            }
        }
    }
    printf("Enclosed Squares: %d\n", enclosedSquares);

    return 0;
}
