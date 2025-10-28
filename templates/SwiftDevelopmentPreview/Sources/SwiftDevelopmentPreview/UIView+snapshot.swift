#if canImport(UIKit)
import UIKit

extension UIView {
    func snapshot() {
        // Use screen size as default, or the view's current size if it's already laid out
        let targetSize: CGSize
        if bounds.size.width > 0 && bounds.size.height > 0 {
            targetSize = bounds.size
        } else {
            // Default to iPhone 15 Pro size for consistent previews
            targetSize = CGSize(width: 393, height: 852)
        }
        let bounds = CGRect(origin: .zero, size: targetSize)

        let renderer = UIGraphicsImageRenderer(bounds: bounds)
        let image = renderer.image { context in
            drawHierarchy(in: bounds, afterScreenUpdates: true)
        }

        guard let data = image.pngData() else {
            print("Failed to convert image to PNG data")
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
