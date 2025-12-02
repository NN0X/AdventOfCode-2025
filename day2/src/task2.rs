use std::fs;

fn compute_repetitions(begin: u64, end: u64) -> Vec<u64>
{
        let mut results = Vec::new();

        for n in begin..=end
        {
                let num_digits = (n as f64).log10().floor() as u64 + 1;
                let half_num_digits = num_digits / 2;

                for current_digit in 1..=half_num_digits
                {
                        if num_digits % current_digit != 0
                        {
                                continue;
                        }

                        let current_digit_reversed = (num_digits - current_digit) as u32;
                        let current_num = n / 10_u64.pow(current_digit_reversed);

                        let num_repeatitions = num_digits / current_digit;
                        let mut constructed_n = 0_u64;
                        for i in 0..num_repeatitions
                        {
                                constructed_n += current_num * 10_u64.pow((current_digit * i) as u32);
                        }

                        if constructed_n == n
                        {
                                results.push(n);
                                break;
                        }
                }

        }

        return results;
}

fn main()
{
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
