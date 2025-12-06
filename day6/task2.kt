import java.io.File

fun parseOps(lines: List<String>): Pair<ArrayList<String>, ArrayList<Int>>
{
        var ops = ArrayList<String>();
        var columnLens = ArrayList<Int>();
        var len = 0;
        val lastLine = lines.last();
        for (i in 0..<lastLine.length)
        {
                val char = lastLine[i];
                if (char != ' ')
                {
                        ops.add(char.toString());
                        if (i != 0)
                        {
                                columnLens.add(len - 1);
                                len = 0;
                        }
                }
                len++;
        }
        columnLens.add(len);

        return Pair(ops, columnLens);
}

fun parseNums(lines: List<String>, columnLens: ArrayList<Int>): ArrayList<ArrayList<Long>>
{
        var nums = ArrayList<ArrayList<Long>>();

        var linesStrs = ArrayList<ArrayList<String>>();

        var isPrevSpace = false;
        for (i in 0..<lines.size)
        {
                var line = lines[i];
                var strs = ArrayList<String>();
                var str = "";
                var curPos = 0;
                var curColumn = 0;
                for (i in 0..<line.length)
                {
                        if (i != 0 && curPos != 0 && curPos % columnLens[curColumn] == 0)
                        {
                                strs.add(str);
                                str = "";
                                curPos = 0;
                                curColumn++;
                        }
                        else
                        {
                                str+=line[i];
                                curPos++;
                        }
                }
                strs.add(str);

                linesStrs.add(strs);
        }

        for (i in 0..<linesStrs[0].size)
        {
                var column = ArrayList<Long>();
                for (j in 0..<columnLens[i])
                {
                        var str = "";
                        for (k in 0..<linesStrs.size)
                        {
                                val char = linesStrs[k][i][j];
                                if (char != ' ')
                                {
                                        str+=char.toString();
                                }
                        }
                        column.add(str.toLong());
                }
                nums.add(column);
        }

        return nums;
}

fun evalOperations(ops: ArrayList<String>, opNums: ArrayList<ArrayList<Long>>): ArrayList<Long>
{
        var results = ArrayList<Long>();

        for (i in 0..<ops.size)
        {
                if (ops[i] == "*")
                {
                        var resultMul = 1L;
                        for (j in 0..<opNums[i].size)
                        {
                                resultMul*=opNums[i][j];
                        }
                        results.add(resultMul);
                }
                else
                {
                        var resultAdd = 0L;
                        for (j in 0..<opNums[i].size)
                        {
                                resultAdd+=opNums[i][j];
                        }
                        results.add(resultAdd);
                }
        }

        return results;
}

fun main()
{
        val file = File("../inputs/day6");
        val data = file.readText();
        val lines = data.split('\n');

        val linesSanitized = lines.take(lines.size-1);
        val linesWithoutOps = linesSanitized.take(linesSanitized.size - 1);

        val (ops, columnLens) = parseOps(linesSanitized);
        val nums = parseNums(linesWithoutOps, columnLens);

        val results = evalOperations(ops, nums);
        val result = results.sum();

        println(result);
}
