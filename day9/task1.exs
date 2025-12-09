defmodule Task1 do
        def getBiggestRectangle(points) do
                rectangles = for pointA <- points, pointB <- points do
                        [pointA, pointB]
                end

                rectanglesWithArea = for rectangle <- rectangles do
                        [[x1, y1], [x2, y2]] = rectangle
                        [rectangle, abs(x1 - x2 + 1) * abs(y1 - y2 + 1)]
                end

                Enum.max_by(rectanglesWithArea, fn [_rectangle, area] -> area end)
        end

        def main() do
                data = File.read!("../inputs/day9")
                lines = String.split(data, "\n", trim: true)

                points = for line <- lines do
                        split = String.split(line, ",", trim: true)
                        x = String.to_integer(Enum.at(split, 0))
                        y = String.to_integer(Enum.at(split, 1))
                        [x, y]
                end

                biggestRectangle = getBiggestRectangle(points)
                IO.puts(Enum.at(biggestRectangle, 1))
        end
end

Task1.main()
