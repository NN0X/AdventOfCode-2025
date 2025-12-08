// swift-tools-version: 6.2
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "day8",
    dependencies: [
        .package(url: "https://github.com/apple/swift-numerics", from: "1.0.0"), 
    ],
    targets: [
        .executableTarget(
            name: "task1",
            dependencies: [
                .product(name: "Numerics", package: "swift-numerics")
            ]
        ),
        .executableTarget(
            name: "task2",
            dependencies: [
                .product(name: "Numerics", package: "swift-numerics")
            ]
        ),
    ]
)
