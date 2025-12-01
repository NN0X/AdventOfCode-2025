#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void process(int* zeros, int* counter, char* line)
{
        int sign = 1;
        if (line[0] == 'L')
        {
                sign = -1;
        }

        line++;
        int mov = atoi(line);

        for (int i = 0; i < mov; i++)
        {
                (*counter)+=sign;
                if (*counter == -1)
                {
                        *counter = 99;
                }
                else if (*counter == 100)
                {
                        *counter = 0;
                }
                if (*counter == 0)
                {
                        (*zeros)++;
                }
        }
}

int main()
{
        FILE* file = fopen("../../inputs/day1", "r");
        if (file == NULL)
        {
                return 1;
        }

        fseek(file, 0, SEEK_END);
        size_t size = ftell(file);
        fseek(file, 0, SEEK_SET);

        int zeros = 0;
        int counter = 50;

        size_t pos = 0;
        size_t index = 0;
        char* line = (char*)malloc(1024);
        while (pos < size)
        {
                pos++;
                char c;
                fread(&c, 1, 1, file);
                if (c == '\n')
                {
                        line[index] = '\0';
                        process(&zeros, &counter, line);
                        index = 0;
                        continue;
                }
                line[index] = c;
                index++;
        }

        printf("Zeros: %d\nCounter: %d\n", zeros, counter);

        return 0;
}
