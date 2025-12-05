import std.stdio;
import std.file;
import std.string;
import std.array;
import std.conv;

long[2] parseFreshRange(string line)
{
        string[2] pairStr;
        long[2] pairInt;
        pairStr = split(line, '-');
        pairInt[0] = pairStr[0].to!long;
        pairInt[1] = pairStr[1].to!long;

        return pairInt;
}

long parseID(string line)
{
        return line.to!long;
}

long countFresh(long[2][] ranges, long[] ids)
{
        long result = 0;
        foreach (long id; ids)
        {
                foreach (ref range; ranges)
                {
                        if (id >= range[0] && id <= range[1])
                        {
                                result++;
                                break;
                        }
                }
        }

        return result;
}

void main()
{
        string data = readText("../../inputs/day5");
        string[] lines = splitLines(data);

        long[2][] ranges;
        long[] ids;

        bool isInFreshSection = true;
        foreach (ref line; lines)
        {
                if (line.length == 0)
                {
                        isInFreshSection = false;
                        continue;
                }

                if (isInFreshSection)
                {
                        ranges ~= parseFreshRange(line);
                }
                else
                {
                        ids ~= parseID(line);
                }
        }

        writeln(countFresh(ranges, ids));
}
