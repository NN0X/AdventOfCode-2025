#include <iostream>
#include <fstream>
#include <vector>
#include <cstdint>
#include <bitset>


struct Space
{
        int width;
        int height;
        std::vector<int> blockIndexes;
};

int main()
{
        std::string filepath = "test";
        std::ifstream file(filepath);

        std::vector<Space> spaces;
        std::vector<uint16_t> blocks;

        std::string currentBlock = "";
        std::string line;
        bool firstLine = true;
        while (std::getline(file, line))
        {
                if (line.empty())
                        continue;
                if (firstLine)
                {
                        firstLine = false;
                        continue;
                }
                if (line[1] == ':')
                {
                        uint16_t block = 0;

                        for (int i = 0; i < currentBlock.size(); i++)
                        {
                                if (currentBlock[i] == '#')
                                {
                                        block |= 1 << i;
                                }
                        }
                        blocks.push_back(block);
                        currentBlock = "";
                        continue;
                }
                else if (line[1] == 'x')
                {
                        continue;
                }

                currentBlock += line;
        }

        for (uint16_t block : blocks)
        {
                std::cout << std::bitset<9>(block) << "\n";
        }
}
