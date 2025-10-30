import Foundation
@_exported import Inject

public enum SwiftDevelopmentPreview {
    public static var isInPreview: Bool {
        ProcessInfo.processInfo.arguments.contains("--swift-development-preview")
    }

    // Track if hot reload is enabled to avoid exit(0)
    public static var isHotReloadEnabled: Bool = false

    // Track if hot reload has been initialized (to prevent multiple initializations)
    private(set) static var hotReloadInitialized: Bool = false

    // Reset the hot reload initialization flag
    static func resetHotReloadInitialization() {
        hotReloadInitialized = false
    }

    // Mark hot reload as initialized
    static func markHotReloadInitialized() {
        hotReloadInitialized = true
    }

    public static var previewPath: String? {
        let args = ProcessInfo.processInfo.arguments
        for (index, arg) in args.enumerated() {
            if arg.hasPrefix("--preview-path=") {
                return String(arg.dropFirst("--preview-path=".count))
            } else if arg == "--preview-path" && index + 1 < args.count {
                return args[index + 1]
            }
        }
        return nil
    }

    public static var previewViewName: String? {
        let args = ProcessInfo.processInfo.arguments
        for (index, arg) in args.enumerated() {
            if arg.hasPrefix("--preview-view=") {
                return String(arg.dropFirst("--preview-view=".count))
            } else if arg == "--preview-view" && index + 1 < args.count {
                return args[index + 1]
            }
        }
        return nil
    }

    public static var previewIndex: Int? {
        let args = ProcessInfo.processInfo.arguments
        for (index, arg) in args.enumerated() {
            if arg.hasPrefix("--preview-index=") {
                return Int(String(arg.dropFirst("--preview-index=".count)))
            } else if arg == "--preview-index" && index + 1 < args.count {
                return Int(args[index + 1])
            }
        }
        return nil
    }

    public static var previewScale: CGFloat? {
        let args = ProcessInfo.processInfo.arguments
        for (index, arg) in args.enumerated() {
            if arg.hasPrefix("--preview-scale=") {
                return CGFloat(Double(String(arg.dropFirst("--preview-scale=".count))) ?? 2.0)
            } else if arg == "--preview-scale" && index + 1 < args.count {
                return CGFloat(Double(args[index + 1]) ?? 2.0)
            }
        }
        return nil
    }
}
