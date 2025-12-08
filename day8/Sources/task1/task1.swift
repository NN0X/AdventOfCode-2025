import Foundation
import Numerics

struct Point3D
{
        var x = 0
        var y = 0
        var z = 0

        init(_ x: Int, _ y: Int, _ z: Int)
        {
                self.x = x
                self.y = y
                self.z = z
        }
}

func getDistance(a: Point3D, b: Point3D) -> Int
{
        let x2 = pow(a.x - b.x, 2)
        let y2 = pow(a.y - b.y, 2)
        let z2 = pow(a.z - b.z, 2)
        let distance = sqrt(x2 + y2 + z2)

        return distance
}

@main
struct task1
{

        static func main()
        {
                var data = ""
                do
                {
                        data = try String(contentsOfFile: "test", encoding: .utf8)
                }
                catch
                {
                        print("Error")
                        return
                }

                let lines = data.components(separatedBy: "\n")
                var points: [Point3D] = []

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

                        points.append(Point3D(x, y, z))
                }
        }
}
