#include <iostream>
#include "inttypes.h"

int main()
{
    FILE * f = fopen("day1/day1_data.txt", "r");

    char * line = nullptr;
    size_t len;

    uint32_t size = 0;
    while (getline(&line, &len, f) != -1)
    {
        uint32_t first = -1;
        uint32_t last = -1;
        char * lineChar = line;
        while(*lineChar)
        {
            if (isdigit(*lineChar))
            {
                if (first == -1)
                {
                    first = *lineChar - '0';
                    last = *lineChar - '0';
                }
                else
                {
                    last = *lineChar - '0';
                }
            }
            lineChar++;
        }
        printf("%d%d\n", first, last);
        size += ((first * 10) + last);
    }
    free(line);

    printf("Sum: %d\n", size);

    return 0;
}
