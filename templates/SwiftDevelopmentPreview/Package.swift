// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "SwiftDevelopmentPreview",
    platforms: [
        .iOS(.v14),
        .macOS(.v11),
        .tvOS(.v14),
        .watchOS(.v6),
        .visionOS(.v1)
    ],
    products: [
        .library(
            name: "SwiftDevelopmentPreview",
            targets: ["SwiftDevelopmentPreview"]
        )
    ],
    dependencies: [
        .package(url: "https://github.com/krzysztofzablocki/Inject.git", from: "1.5.2")
    ],
    targets: [
        .target(
            name: "SwiftDevelopmentPreview",
            dependencies: ["Inject"],
            linkerSettings: [
                .unsafeFlags(["-Xlinker", "-interposable"], .when(configuration: .debug))
            ]
        )
    ]
)
