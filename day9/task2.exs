defmodule Task2 do
        def generateEdges(points) do
                edges = Enum.chunk_every(points, 2, 1, :discard)
                        |> Enum.map(fn [p1, p2] -> {p1, p2} end)
                        |> Kernel.++([{List.last(points), List.first(points)}])
                edges
        end

        def isPointOnEdge({px, py}, {{x1, y1}, {x2, y2}}) do
                cond do
                        x1 == x2 and px == x1 and py >= min(y1, y2) and py <= max(y1, y2) -> true
                        y1 == y2 and py == y1 and px >= min(x1, x2) and px <= max(x1, x2) -> true
                        true -> false
                end
        end

        def isPointOnBoundary(point, edges) do
                Enum.any?(edges, fn edge -> isPointOnEdge(point, edge) end)
        end

        def countRayIntersections({px, py}, edges) do
                intersections = Enum.filter(edges, fn {p1, p2} ->
                        {x1, y1} = p1
                        {x2, y2} = p2

                        cond do
                                x1 == x2 -> false
                                px < min(x1, x2) or px >= max(x1, x2) -> false
                                true ->
                                        yIntersect = y1 + (px - x1) * (y2 - y1) / (x2 - x1)
                                        yIntersect > py
                        end

                end)

                length(intersections)
        end

        def isPointInPolygon({px, py}, edges) do
                onBoundary = isPointOnBoundary({px, py}, edges)
                intersections = countRayIntersections({px, py}, edges)
                onBoundary or rem(intersections, 2) == 1
        end

        def doOrthogonalSegmentsIntersect({{ax1, ay1}, {ax2, ay2}}, {{bx1, by1}, {bx2, by2}}) do
                cond do
                        ay1 == ay2 and bx1 == bx2 ->
                                bx1 > min(ax1, ax2) and bx1 < max(ax1, ax2) and
                                ay1 > min(by1, by2) and ay1 < max(by1, by2)
                        ax1 == ax2 and by1 == by2 ->
                                ax1 > min(bx1, bx2) and ax1 < max(bx1, bx2) and
                                by1 > min(ay1, ay2) and by1 < max(ay1, ay2)
                        true -> false
                end
        end

        def isRectangleWithin({{x1, y1}, {x2, y2}}, edges) do
                minX = min(x1, x2)
                maxX = max(x1, x2)
                minY = min(y1, y2)
                maxY = max(y1, y2)
                corners = [
                        {minX, minY},
                        {minX, maxY},
                        {maxX, minY},
                        {maxX, maxY}
                ]
                allCornersInside = Enum.all?(corners, fn corner -> 
                        isPointInPolygon(corner, edges)
                end)
                if not allCornersInside do
                        false
                else
                        polygonVertices = edges 
                                |> Enum.map(fn {p1, _p2} -> p1 end)
                                |> Enum.uniq()
                        verticesInside = Enum.filter(polygonVertices, fn {vx, vy} ->
                                vx > minX and vx < maxX and vy > minY and vy < maxY
                        end)
                        if verticesInside != [] do
                                false
                        else
                                rectEdges = [
                                        {{minX, minY}, {maxX, minY}},
                                        {{minX, maxY}, {maxX, maxY}},
                                        {{minX, minY}, {minX, maxY}},
                                        {{maxX, minY}, {maxX, maxY}}
                                ]
                                intersectingEdges = Enum.filter(rectEdges, fn rectEdge ->
                                        Enum.any?(edges, fn polyEdge ->
                                                doOrthogonalSegmentsIntersect(rectEdge, polyEdge)
                                        end)
                                end)
                                if intersectingEdges != [] do
                                        false
                                else
                                        true
                                end
                        end
                end
        end

        def getBiggestRectangleWithinLimits(points) do
                rectangles = for {x1, y1} = pointA <- points, {x2, y2} = pointB <- points, 
                                 pointA != pointB, x1 != x2, y1 != y2 do
                        {pointA, pointB}
                end

                rectanglesWithArea = for rectangle <- rectangles do
                        {{x1, y1}, {x2, y2}} = rectangle
                        {rectangle, (abs(x1 - x2) + 1) * (abs(y1 - y2) + 1)}
                end

                sortedRectangles = Enum.sort_by(rectanglesWithArea, fn {_rect, area} -> -area end)

                edges = generateEdges(points)

                result = Enum.find(sortedRectangles, fn {rect, area} ->
                        area > 0 and isRectangleWithin(rect, edges)
                end)

                case result do
                        nil ->
                                {{0, 0}, {0, 0}, 0}
                        found ->
                                found
                end
        end

        def main() do
                data = File.read!("../inputs/day9")
                lines = String.split(data, "\n", trim: true)
                redPoints = for line <- lines do
                        [x, y | _] = String.split(line, ",", trim: true) |> Enum.map(&String.to_integer/1)
                        {x, y}
                end

                biggestRectangleWithin = getBiggestRectangleWithinLimits(redPoints)

                IO.inspect(biggestRectangleWithin)
        end
end

Task2.main()
