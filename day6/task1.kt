import java.io.File

fun parseData(lines: List<String>): Pair<ArrayList<ArrayList<Long>>, ArrayList<String>>
{
        var operationNums = ArrayList<ArrayList<Long>>();
        var ops = ArrayList<String>();

        val numsLen = lines.size - 1;
        for (i in 0..<numsLen)
        {
                var nums = ArrayList<Long>();
                for (num in lines[i].split(' '))
                {
                        if (num == "")
                        {
                                continue;
                        }

                        nums.add(num.toLong());
                }
                operationNums.add(nums);
        }

        for (op in lines.last())
        {
                if (op != ' ')
                {
                        ops.add(op.toString());
                }
        }

        return Pair(operationNums, ops);
}

fun evalOperations(ops: ArrayList<String>, opNums: ArrayList<ArrayList<Long>>): ArrayList<Long>
{
        var results = ArrayList<Long>();

        for (i in 0..<ops.size)
        {
                if (ops[i] == "*")
                {
                        var resultMul = 1L;
                        for (j in 0..<opNums.size)
                        {
                                resultMul*=opNums[j][i];
                        }
                        results.add(resultMul);
                }
                else
                {
                        var resultAdd = 0L;
                        for (j in 0..<opNums.size)
                        {
                                resultAdd+=opNums[j][i];
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

        val (nums, ops) = parseData(lines.take(lines.size-1));
        val results = evalOperations(ops, nums);
        val result = results.sum();

        println(result);

        //println(lines);
}
