#include <cmath>
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

struct Card
{
    int ID;
    std::vector<int> winners;
    std::vector<int> numbers;
    int matches;
};

int countingCards (std::vector<Card> & cards, int index)
{
    if (cards.at(index).matches == 0)
        return 0;

    int cardCount = cards.at(index).matches;
    for (int i = 0; i < cards.at(index).matches; i++)
    {
        cardCount += countingCards(cards, index + 1 + i);
    }

    return cardCount;
}

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
    FILE * f = fopen("day4/day4.txt", "r");

    // std::string test("tqqqqqasdasdas  sads dsa s   asdasdasdas yyuyuyr    ");
    // trim(test);
    // printf(":%s:\n", test.c_str());
    // return 1;

    std::vector<Card> cards;

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
            int cardID;
            sscanf(strLine.c_str(), "Card %d:", &cardID);
            Card card;
            card.ID = cardID;

            // Split into winning numbers and drawn numbers
            std::string draws = strLine.substr(colon+1);
            trim(draws);
            auto eachDraws = split(draws, "|");
            card.winners = parseNumbers(eachDraws.at(0));
            card.numbers = parseNumbers(eachDraws.at(1));

            cards.push_back(card);
        }
    }
    free(line);

    // Print out all the cards again
    for (auto card : cards)
    {
        printf("Card %d: ", card.ID);
        for (auto winner : card.winners)
        {
            printf("%d ", winner);
        }
        printf("| ");
        for (auto number : card.numbers)
        {
            printf("%d ", number);
        }
        printf("\n");
    }

    int score = 0;
    for (auto card : cards)
    {
        int cardScorePow = 0;
        for (auto winner : card.winners)
        {
            for (auto number : card.numbers)
            {
                if (winner == number)
                    cardScorePow++;
            }
        }
        if (cardScorePow > 0)
            score += std::pow(2, cardScorePow-1);
    }
    printf("Total score: %d\n", score);


    for (auto &card : cards)
    {
        int matches = 0;
        for (auto winner : card.winners)
        {
            for (auto number : card.numbers)
            {
                if (winner == number)
                    matches++;
            }
        }
        card.matches = matches;
    }

    int totalCardCount = 0;
    for (int i = 0; i < cards.size(); i++)
    {
        //printf("Card: %d\n", i+1);
        totalCardCount += countingCards(cards, i);
    }
    totalCardCount += cards.size();
    printf("Total cards: %d\n", totalCardCount);

    return 0;
}
