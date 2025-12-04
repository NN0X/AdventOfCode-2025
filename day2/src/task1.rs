use std::fs;

fn compute_repetitions(begin: u64, end: u64) -> Vec<u64>
{
        let mut results = Vec::new();

        for n in begin..=end
        {
                let num_digits = (n as f64).log10().floor() as u64 + 1;
                let half_num_digits : u64 = num_digits / 2;
                let upper_num = n / 10_u64.pow(half_num_digits as u32);
                let lower_num = n - upper_num * 10_u64.pow(half_num_digits as u32);

                if upper_num == lower_num
                {
                        results.push(n);
                }
        }

        return results;
}

fn main()
{
        // INFO: input has to have , at the end
        let data = fs::read_to_string("../inputs/day2").expect("Unable to read file");

        let mut ranges = Vec::new();
        let mut pair = [0, 0];

        let data_chars = data.chars();
        let mut str = String::new();

        for c in data_chars
        {
                if c == '-'
                {
                        pair[0] = str.parse::<u64>().unwrap();
                        str.clear();
                }
                else if c == ','
                {
                        pair[1] = str.parse::<u64>().unwrap();
                        ranges.push(pair);
                        str.clear()
                }
                else
                {
                        str.push(c);
                }
        }


        let mut results = Vec::new();
        for pair in ranges
        {
                results.extend(compute_repetitions(pair[0], pair[1]));
        }

        let sum : u64 = results.iter().sum();
        println!("Sum: {}\n", sum);
}
