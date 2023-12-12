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

struct Number
{
    int number;
    int x;
    int y;
    int digits;
};

struct SpecialChar
{
    char c;
    int x;
    int y;
};

int main()
{
    FILE * f = fopen("day3/day3.txt", "r");

    // std::string test("tqqqqqasdasdas  sads dsa s   asdasdasdas yyuyuyr    ");
    // trim(test);
    // printf(":%s:\n", test.c_str());
    // return 1;

    std::vector<Number> numbers;
    std::vector<SpecialChar> specialChars;

    char * line = nullptr;
    size_t len;

    uint32_t size = 0;
    int y = 0;
    while (getline(&line, &len, f) != -1)
    {
        std::string strLine = std::string(line);
        strLine = std::regex_replace(strLine, std::regex("\n"), "");
        std::cout << strLine << std::endl;

        // walk line getting numbers
        const char * cLine = strLine.c_str();
        int x = 0;
        int numStart = -1;
        while (*cLine)
        {
            if (isdigit(*cLine))
            {
                if (numStart == -1)
                {
                    numStart = x;
                }
            }
            else if (!isdigit(*cLine) && numStart != -1)
            {
                Number num{};
                num.number = std::stoi(strLine.substr(numStart, x - numStart));
                num.x = numStart;
                num.y = y;
                num.digits = x - numStart;
                numbers.push_back(num);

                numStart = -1;
            }

            // Add special chars
            if (!isdigit(*cLine) && *cLine != '.')
            {
                SpecialChar specialChar{};
                specialChar.c = *cLine;
                specialChar.x = x;
                specialChar.y = y;
                specialChars.push_back(specialChar);
            }

            cLine++;
            x++;
        }

        // end of line, close off a number
        if (numStart != -1)
        {
            Number num{};
            num.number = std::stoi(strLine.substr(numStart, x - numStart));
            num.x = numStart;
            num.y = y;
            num.digits = x - numStart;
            numbers.push_back(num);

            numStart = -1;
        }

        y++;
    }
    free(line);

    // print numbers
    for (auto n : numbers)
    {
        printf("%d at [%d, %d] digits = %d\n", n.number, n.x, n.y, n.digits);
    }
    for (auto spc : specialChars)
    {
        printf("'%c' at [%d, %d]\n", spc.c, spc.x, spc.y);
    }

    // sum adjacent numbers
    int adjacentSum = 0;
    for (auto n : numbers)
    {
        bool hasAdjacent = false;
        for (auto spc : specialChars)
        {
            if (spc.x >= n.x-1 && spc.x <= n.x+n.digits &&
                spc.y >= n.y-1 && spc.y <= n.y+1)
            {
                hasAdjacent = true;
                printf("%d has adjacent!\n", n.number);
                break;
            }
        }
        if (hasAdjacent)
            adjacentSum += n.number;
    }
    printf("Adjacent sum: %d\n", adjacentSum);

    // gear ratio
    int sumGearRatio = 0;
    for (auto spc : specialChars)
    {
        if (spc.c == '*')
        {
            int adjNumbers = 0;
            int gearRatio = 1;
            for (auto n : numbers)
            {
                if (spc.x >= n.x-1 && spc.x <= n.x+n.digits &&
                    spc.y >= n.y-1 && spc.y <= n.y+1)
                {
                    adjNumbers++;
                    gearRatio *= n.number;
                }
            }
            if (adjNumbers == 2)
                sumGearRatio += gearRatio;
        }
    }
    printf("Gear ratio sum: %d\n", sumGearRatio);

    return 0;
}
