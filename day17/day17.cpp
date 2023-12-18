#include <cmath>
#include <iostream>
#include <numeric>
#include <regex>
#include <variant>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <inttypes.h>

#define SHOW_OUTPUT 1

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

enum Direction {
    UP,
    DOWN,
    LEFT,
    RIGHT,
    NONE
};

struct Node {
    int weight;
    int distance[4][3];
    bool visited[4][3]; // [dir][count]
    bool explored[4][3];
    Direction inDir;
    int inDirCount;
};

struct Location {
    int x;
    int y;
    Direction dir;
    int dirCount;
};

int main()
{
    FILE * f = fopen("day17/day17.txt", "r");

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

    std::vector<std::vector<Node>> board(boardSizeX, std::vector<Node>(boardSizeY, {0, INT32_MAX, {false}, NONE, 0}));

    for (int j = 0; j < boardSizeY; j++)
    {
        for (int i = 0; i < boardSizeX; i++)
        {
            Node n = {std::stoi(lines.at(j).substr(i, 1)), INT32_MAX, {false}, NONE, 0};
            for (int k = 0; k < 4; k++)
            {
                for (int m = 0; m < 3; m++)
                {
                    n.distance[k][m] = INT32_MAX;
                    n.visited[k][m] = false;
                    n.explored[k][m] = false;
                }
            }
//            if (j == 0)
//                n.visited[DOWN][0] = true; n.visited[DOWN][1] = true; n.visited[DOWN][2] = true;
//            if (j == boardSizeY-1)
//                n.visited[UP][0] = true; n.visited[UP][1] = true; n.visited[UP][2] = true;
//            if (i == boardSizeX-1)
//                n.visited[RIGHT][0] = true; n.visited[RIGHT][1] = true; n.visited[RIGHT][2] = true;
//            if (i == 0)
//                n.visited[LEFT][0] = true; n.visited[LEFT][1] = true; n.visited[LEFT][2] = true;
            board[i][j] = n;
        }
    }

    printf("------------\n");
    for (int i = 0; i < boardSizeY; i++)
    {
        for (int j = 0; j < boardSizeX; j++)
        {
            printf("%d", board[j][i].weight);
        }
        printf("\n");
    }

    for (int k = 0; k < 4; k++)
    {
        board[0][0].explored[k][0] = true;
        for (int m = 0; m < 3; m++)
        {
            board[0][0].distance[k][m] = 0;
        }
    }

    Location l = {0, 0, NONE, 0};

    while (true)
    {
        // Find lowest distance unvisited node
        int minDistX = boardSizeX;
        int minDistY = boardSizeY;
        int minDistDir = NONE;
        int minDistCount = 0;
        int minDist = INT32_MAX;
        bool hasUnvisited = false;
        for (int i = 0; i < boardSizeX; i++)
        {
            for (int j = 0; j < boardSizeY; j++)
            {
                for (int k = 0; k < 4; k++)
                {
                    for (int m = 0; m < 3; m++)
                    {
//                        if (i == 0 && j == 0)
//                        {
//                            //printf("Visited %d %d %b\n", k, m, board[i][j].visited[k][m]);
//                        }
                        if (!board[i][j].visited[k][m] && board[i][j].explored[k][m] && board[i][j].distance[k][m] < minDist)
                        {
                            minDistX = i;
                            minDistY = j;
                            minDistDir = k;
                            minDistCount = m;
                            minDist = board[i][j].distance[k][m];
                            hasUnvisited = true;
                        }
                    }
                }
            }
        }
        if (!hasUnvisited || minDist == INT32_MAX) {
            if (SHOW_OUTPUT) printf("Next Node: [%d, %d]\n", minDistX, minDistY);
            break;
        }

        l.x = minDistX;
        l.y = minDistY;
        l.dir = static_cast<Direction>(minDistDir);
        l.dirCount = minDistCount;
        // set current visited
        board[l.x][l.y].visited[l.dir][l.dirCount] = true;

        if (l.dir == UP)            {if (SHOW_OUTPUT) printf("At [%d, %d] UP %d %d\n", l.x, l.y, l.dirCount, minDist);}
        else if (l.dir == DOWN)     {if (SHOW_OUTPUT) printf("At [%d, %d] DOWN %d %d\n", l.x, l.y, l.dirCount, minDist);}
        else if (l.dir == LEFT)     {if (SHOW_OUTPUT) printf("At [%d, %d] LEFT %d %d\n", l.x, l.y, l.dirCount, minDist);}
        else if (l.dir == RIGHT)    {if (SHOW_OUTPUT) printf("At [%d, %d] RIGHT %d %d\n", l.x, l.y, l.dirCount, minDist);}

        // check neighbours
        std::vector<Direction> neighbourDirections;
        if (l.x < boardSizeX-1 && !(l.dir == RIGHT && l.dirCount == 2) && l.dir != LEFT)
            neighbourDirections.push_back(RIGHT);
        if (l.x > 0 && !(l.dir == LEFT && l.dirCount == 2) && l.dir != RIGHT)
            neighbourDirections.push_back(LEFT);
        if (l.y > 0 && !(l.dir == UP && l.dirCount == 2) && l.dir != DOWN)
            neighbourDirections.push_back(UP);
        if (l.y < boardSizeY-1 && !(l.dir == DOWN && l.dirCount == 2) && l.dir != UP)
            neighbourDirections.push_back(DOWN);

        for (auto nd : neighbourDirections)
        {
            if (SHOW_OUTPUT) printf("%d::%d\n", nd, l.dir);
            Node n;
            int newDirCount = 0;
            if (static_cast<Direction>(nd) == l.dir)
                newDirCount += l.dirCount + 1;
            switch(nd)
            {
                case UP:
                    n = board[l.x][l.y-1];
                    break;
                case DOWN:
                    n = board[l.x][l.y+1];
                    break;
                case LEFT:
                    n = board[l.x-1][l.y];
                    break;
                case RIGHT:
                    n = board[l.x+1][l.y];
                    break;
            }

            if (!n.visited[nd][newDirCount])
            {
                n.explored[nd][newDirCount] = true;
                int altDist = board[l.x][l.y].distance[l.dir][l.dirCount] + n.weight;
                if (altDist < n.distance[nd][newDirCount])
                {
                    if (SHOW_OUTPUT) std::cout << "New Dir count: " << newDirCount << std::endl;
                    if (SHOW_OUTPUT) std::cout << "Alt dist: " << altDist << std::endl;
                    n.distance[nd][newDirCount] = altDist;
                }
            }

            switch(nd)
            {
                case UP:
                    board[l.x][l.y-1] = n;
                    break;
                case DOWN:
                    board[l.x][l.y+1] = n;
                    break;
                case LEFT:
                    board[l.x-1][l.y] = n;
                    break;
                case RIGHT:
                    board[l.x+1][l.y] = n;
                    break;
            }
        }
        if (SHOW_OUTPUT) printf("---\n");
    }

    printf("------------\n");
    for (int i = 0; i < boardSizeY; i++)
    {
        for (int j = 0; j < boardSizeX; j++)
        {
            int mind = INT32_MAX;
            for (int k = 0; k < 4; k++)
            {
                for (int m = 0; m < 3; m++)
                {
                    if (board[j][i].distance[k][m] < mind)
                        mind = board[j][i].distance[k][m];
                }
            }
            printf("%i,", mind);
        }
        printf("\n");
    }

//    int curX = boardSizeX-1;
//    int curY = boardSizeY-1;
//    Node n = board[curX][curY];
//    board[curX][curY].distance = 99;
//    while (n.inDir != NONE)
//    {
//        switch(n.inDir)
//        {
//            case UP:
//                curY+=1;
//                break;
//            case DOWN:
//                curY-=1;
//                break;
//            case LEFT:
//                curX+=1;
//                break;
//            case RIGHT:
//                curX-=1;
//                break;
//        }
//        printf("[%d, %d]\n", curX, curY);
//        board[curX][curY].distance = 666;
//        n = board[curX][curY];
//    }
//
//    printf("------------\n");
//    for (int i = 0; i < boardSizeY; i++)
//    {
//        for (int j = 0; j < boardSizeX; j++)
//        {
//            if (board[j][i].distance == 666)
//            {
//                printf(">");
//            }
//            else
//            {
//                printf("%i", board[j][i].weight);
//            }
//        }
//        printf("\n");
//    }


    return 0;
}
