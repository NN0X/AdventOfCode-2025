import std.stdio;
import std.file;
import std.string;
import std.array;
import std.conv;
import std.algorithm;

long[2] parseFreshRange(string line)
{
        string[2] pairStr;
        long[2] pairInt;
        pairStr = split(line, '-');
        pairInt[0] = pairStr[0].to!long;
        pairInt[1] = pairStr[1].to!long;

        return pairInt;
}

long countFresh(long[2][] ranges)
{
        long result = 0;
        foreach (ref range; ranges)
        {
                result += range[1] - range[0] + 1;
        }

        return result;
}

long[2][] mergeRanges(ref long[2][] ranges)
{

        long[2][] resultArr;

        alias comp = (a, b) => a[0] < b[0];
        ranges = ranges.sort!(comp).release;

        long prevBegin = ranges[0][0];
        long prevEnd = ranges[0][1];
        foreach (long i; 1 .. ranges.length)
        {
                long begin = ranges[i][0];
                long end = ranges[i][1];
                if (begin > prevEnd + 1)
                {
                        resultArr ~= [prevBegin, prevEnd];
                        prevBegin = begin;
                        prevEnd = end;
                }
                else if (end > prevEnd)
                {
                        prevEnd = end;
                }

        }

        resultArr ~= [prevBegin, prevEnd];

        return resultArr;
}

void main()
{
        string data = readText("../../inputs/day5");
        string[] lines = splitLines(data);

        long[2][] ranges;

        foreach (ref line; lines)
        {
                if (line.length == 0)
                {
                        break;
                }
                ranges ~= parseFreshRange(line);
        }

        writeln(countFresh(mergeRanges(ranges)));
}
