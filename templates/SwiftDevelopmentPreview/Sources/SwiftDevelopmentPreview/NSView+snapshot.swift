#if canImport(AppKit)
import AppKit

extension NSView {
    func snapshot() {
        guard let window = window else {
            print("View has no window")
            return
        }

        let backingScaleFactor = window.backingScaleFactor
        let size = fittingSize

        guard let bitmapRep = NSBitmapImageRep(
            bitmapDataPlanes: nil,
            pixelsWide: Int(size.width * backingScaleFactor),
            pixelsHigh: Int(size.height * backingScaleFactor),
            bitsPerSample: 8,
            samplesPerPixel: 4,
            hasAlpha: true,
            isPlanar: false,
            colorSpaceName: .deviceRGB,
            bytesPerRow: 0,
            bitsPerPixel: 0
        ) else {
            print("Failed to create bitmap representation")
            return
        }

        bitmapRep.size = size

        frame = NSRect(origin: .zero, size: size)
        cacheDisplay(in: bounds, to: bitmapRep)

        guard let data = bitmapRep.representation(using: .png, properties: [:]) else {
            print("Failed to convert bitmap to PNG data")
            return
        }

        // Get output path from command line arguments or use default
        let outputPath: String
        if let path = SwiftDevelopmentPreview.previewPath {
            outputPath = path
        } else {
            // Fallback: use bundle name in temp directory
            let bundleName = Bundle.main.bundleURL.deletingPathExtension().lastPathComponent
            outputPath = "/tmp/swift-development-preview-\(bundleName).png"
        }

        let url = URL(fileURLWithPath: outputPath)

        // Ensure parent directory exists
        let directory = url.deletingLastPathComponent()
        try? FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)

        do {
            try data.write(to: url)
            print("Preview saved to: \(outputPath)")
        } catch {
            print("Failed to write preview: \(error)")
        }

        // Exit after saving preview (only if not in hot reload mode)
        if !SwiftDevelopmentPreview.isHotReloadEnabled {
            exit(0)
        }
    }
}
#endif
