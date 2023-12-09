#include <cmath>
#include <iostream>
#include <numeric>
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

struct Node
{
    std::string name;
    std::string left;
    std::string right;
    bool anode;
    bool znode;
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
    FILE * f = fopen("day8/day8.txt", "r");

    // ---------------------------------------------------------
    // our data structures
    std::string instructions;
    std::vector<Node> nodes;
    std::unordered_map<std::string, Node> nodes_map;

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
        if (first_line)
        {
            first_line = false;
            instructions = strLine;
            continue;
        }
        if (strLine.empty())
            continue;


        std::string name;
        std::string left;
        std::string right;
        name = strLine.substr(0, 3);
        size_t index = strLine.find_first_of('(');
        left = strLine.substr(index + 1, 3);
        index = strLine.find_first_of(',');
        right = strLine.substr(index + 2, 3);

        printf("Name: %s\n", name.c_str());
        printf("Left: %s\n", left.c_str());
        printf("Right: %s\n", right.c_str());

        Node newNode;
        newNode.name = name;
        if (newNode.name.at(2) == 'A')
            newNode.anode = true;
        else
            newNode.anode = false;
        if (newNode.name.at(2) == 'Z')
            newNode.znode = true;
        else
            newNode.znode = false;
        newNode.left = left;
        newNode.right = right;
        nodes.push_back(newNode);
    }
    free(line);

    // add nodes to our unordered map so they are easy to traverse
    for (const auto& node : nodes)
    {
        nodes_map[node.name] = node;
    }

    // ---------------------------------------------------------
    // Part 1
    // start at AAA, for left and right until we find ZZZ
    const char * instr = instructions.c_str();
    std::string curLocation = "AAA";
    int steps = 0;
    while (true)
    {
        if (*instr == 'L')
        {
            curLocation = nodes_map[curLocation].left;
        }
        else
        {
            curLocation = nodes_map[curLocation].right;
        }
        steps++;

        if (curLocation == "ZZZ")
        {
            break;
        }

        instr++;
        if (!*instr) instr = instructions.c_str();
    }
    printf("Steps taken: %d\n", steps);

    // ---------------------------------------------------------
    // Part 2
    // for each xxA node, follow left-right until we reach xxZ node
    // find lowest common multiple of all step counts for the xxA nodes
    instr = instructions.c_str();
    std::vector<Node> aNodes;
    for (auto node : nodes)
    {
        if (node.anode)
            aNodes.push_back(node);
    }
    std::vector<int> stepsZed;
    for (auto node : aNodes)
    {
        instr = instructions.c_str();
        int tmpSteps = 0;
        while (true)
        {
            if (*instr == 'L')
            {
                node = nodes_map[node.left];
            }
            else
            {
                node = nodes_map[node.right];
            }
            tmpSteps++;
            if (node.znode)
            {
                stepsZed.push_back(tmpSteps);
                break;
            }
            instr++;
            if (!*instr) instr = instructions.c_str();
        }
    }
    // for (auto step : stepsZed)
    // {
    //     printf("Steps %d\n", step);
    // }
    uint64_t curSize = std::lcm(stepsZed.at(0), stepsZed.at(1));
    for (int i = 2; i < stepsZed.size(); i++)
    {
        curSize = std::lcm(curSize, stepsZed.at(i));
    }
    printf("All Zed steps taken: %lu\n", curSize);


    return 0;
}
