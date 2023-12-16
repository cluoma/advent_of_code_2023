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

uint64_t hash(const char * key)
{
    uint64_t hash = 0;
    char c = *key;
    while (c)
    {
        hash += (uint64_t)c;
        hash *= 17;
        hash %= 256;
        c = *(++key);
    }
    return hash;
}

struct Token {
    std::string label;
    uint64_t labelHash;
    uint64_t focalLength;
};

int main()
{
    FILE * f = fopen("day15/day15.txt", "r");

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

    auto tokens = split(lines.at(0), ",");

    // ---------------------------------------------------------
    // Part 1
    uint64_t hashSum = 0;
    for (auto t : tokens)
    {
        uint64_t h = hash(t.c_str());
        hashSum += h;
        printf("%s :: %lu\n", t.c_str(), h);
    }

    printf("Part 1: %lu\n", hashSum);


    // ---------------------------------------------------------
    // Part 2 - way more convoluted than it needs to be
    std::vector<Token> lightBox[256];
    for (auto t : tokens)
    {
        uint64_t delimPos = t.find_first_not_of("abcdefghijklmnopqrstuvwxyz");
        std::string label = t.substr(0, delimPos);
        uint64_t h = hash(label.c_str());

        if (t.at(delimPos) == '-')
        {
            erase_if(lightBox[h], [&label](const Token & m) -> bool { return m.label == label; });
        }
        else
        {
            bool replaced = false;
            Token tS = {label, h, static_cast<uint64_t>(std::stoi(t.substr(delimPos+1)))};
            std::replace_if(
                    lightBox[h].begin(),
                    lightBox[h].end(),
                    [&label, &replaced](const Token & m) -> bool {
                        if (m.label == label) {
                            replaced = true;
                            return true;
                        }
                        return false;
                    },
                    tS);
            if (!replaced)
                lightBox[h].push_back(tS);
        }
    }

    uint64_t boxValues;
    uint64_t box = 1;
    for (auto m : lightBox)
    {
        for (int i = 0; i < m.size(); i++)
        {
            boxValues += box * (i+1) * m.at(i).focalLength;
        }
        box++;
    }

    printf("Part 2: %lu\n", boxValues);

    return 0;
}
