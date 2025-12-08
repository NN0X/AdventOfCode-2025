import Foundation
import Numerics

struct Point3D : Equatable, Hashable
{
        var id = 0
        var x = 0
        var y = 0
        var z = 0

        init(_ id: Int, _ x: Int, _ y: Int, _ z: Int)
        {
                self.id = id
                self.x = x
                self.y = y
                self.z = z
        }
}

struct Point3DPair : Hashable
{
        let pointA: Int
        let pointB: Int
        let distance: Double

        func hash(into hasher: inout Hasher)
        {
                let minPoint = min(pointA, pointB)
                let maxPoint = max(pointA, pointB)
                hasher.combine(minPoint)
                hasher.combine(maxPoint)
        }

        static func ==(lhs: Point3DPair, rhs: Point3DPair) -> Bool
        {
                return (lhs.pointA == rhs.pointA && lhs.pointB == rhs.pointB) ||
                (lhs.pointA == rhs.pointB && lhs.pointB == rhs.pointA)
        }
}

func getDistance(a: Point3D, b: Point3D) -> Double
{
        let x2 = pow(Double(a.x - b.x), 2)
        let y2 = pow(Double(a.y - b.y), 2)
        let z2 = pow(Double(a.z - b.z), 2)
        let distance = sqrt(x2 + y2 + z2)

        return distance
}

func sortDistances(distances: Set<Point3DPair>) -> [Point3DPair]
{
        let sorted = distances.sorted
                {
                        (pairA, pairB) -> Bool in
                        return pairA.distance < pairB.distance
                }

        return sorted
}

func findParent(parents: inout [Int: Int], point: Int) -> Int
{
        if parents[point] != point
        {
                parents[point] = findParent(parents: &parents, point: parents[point]!)
        }
        return parents[point]!
}

func unionParents(parents: inout [Int: Int], pointA: Int, pointB: Int)
{
        let parentA = findParent(parents: &parents, point: pointA)
        let parentB = findParent(parents: &parents, point: pointB)

        if parentA != parentB
        {
                parents[parentB] = parentA
        }
}

func mergeConnectionsAndReturnLastMeaningful(connections: [[Int]]) -> [Int]
{
        var lastMeaningful: [Int] = []
        var parents: [Int: Int] = [:]

        for connection in connections
        {
                let pointA = connection[0]
                let pointB = connection[1]

                parents[pointA] = pointA
                parents[pointB] = pointB
        }

        for connection in connections
        {
                let pointA = connection[0]
                let pointB = connection[1]

                let parentA = findParent(parents: &parents, point: pointA)
                let parentB = findParent(parents: &parents, point: pointB)

                if parentA != parentB
                {
                        unionParents(parents: &parents, pointA: pointA, pointB: pointB)
                        lastMeaningful = connection
                }
        }

        return lastMeaningful
}

@main
struct task1
{
        static func main()
        {
                var data = ""
                do
                {
                        data = try String(contentsOfFile: "../inputs/day8", encoding: .utf8)
                }
                catch
                {
                        print("Error")
                        return
                }

                let lines = data.components(separatedBy: "\n")
                var points: [Point3D] = []
                var pointsMap: [Int: Point3D] = [:]

                var id = 0
                for line in lines.dropLast()
                {
                        let lineArr = line.components(separatedBy: ",")
                        guard let x = Int(lineArr[0]),
                              let y = Int(lineArr[1]),
                              let z = Int(lineArr[2])
                        else
                        {
                                continue
                        }

                        points.append(Point3D(id, x, y, z))
                        pointsMap[id] = Point3D(id, x, y, z)
                        id+=1
                }

                var distances: Set<Point3DPair> = []

                for i in 0..<points.count
                {
                        for j in (i + 1)..<points.count
                        {
                                let pointA = points[i]
                                let pointB = points[j]
                                let distance = getDistance(a: pointA, b: pointB)
                                distances.insert(Point3DPair(pointA: pointA.id, pointB: pointB.id, distance: distance))
                        }
                }

                let sortedDistances = sortDistances(distances: distances)
                var connections: [[Int]] = []

                var counter = 0
                for pair in sortedDistances
                {
                        let pointA = pair.pointA
                        let pointB = pair.pointB
                        connections.append([pointA, pointB])
                        counter+=1
                }

                let last = mergeConnectionsAndReturnLastMeaningful(connections: connections)

                let x2 = pointsMap[last[0]]!.x * pointsMap[last[1]]!.x
                print(x2)
        }
}
