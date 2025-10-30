#if canImport(SwiftUI)
import SwiftUI

extension View {
    func snapshot() {
        #if canImport(UIKit)
        // Create a temporary hosting controller to measure content
        let tempView = self.fixedSize()
        let tempController = UIHostingController(rootView: tempView)

        // Get the size that fits
        let measuredSize = tempController.sizeThatFits(in: CGSize(width: CGFloat.infinity, height: CGFloat.infinity))

        // Set frame and force layout to get accurate bounds
        tempController.view.frame = CGRect(origin: .zero, size: measuredSize)
        tempController.view.layoutIfNeeded()

        // Get the actual bounds after layout
        let actualBounds = tempController.view.bounds.size

        print("📐 Measured: \(Int(measuredSize.width))x\(Int(measuredSize.height)), Actual: \(Int(actualBounds.width))x\(Int(actualBounds.height))")

        // Calculate final size including padding
        let padding: CGFloat = 20
        let finalSize: CGSize

        if actualBounds.width > 0 && actualBounds.height > 0 &&
           actualBounds.width < 400 && actualBounds.height < 900 {
            // For small views, use fixedSize with padding
            finalSize = CGSize(
                width: actualBounds.width + padding * 2,
                height: actualBounds.height + padding * 2
            )
            print("📐 Final size: \(Int(finalSize.width))x\(Int(finalSize.height)) (content: \(Int(actualBounds.width))x\(Int(actualBounds.height)) + 20pt padding)")
        } else {
            // Fallback for full-screen views
            finalSize = CGSize(width: 393, height: 852)
            print("📐 Using fallback size: \(Int(finalSize.width))x\(Int(finalSize.height))")
        }

        // Create view with fixedSize to prevent expansion, then add padding
        let finalView = self
            .fixedSize()
            .padding(padding)
            .frame(width: finalSize.width, height: finalSize.height, alignment: .center)
            .background(Color.clear)

        let hostingController = UIHostingController(rootView: finalView)
        hostingController.view.frame = CGRect(origin: .zero, size: finalSize)
        hostingController.view.backgroundColor = .clear
        hostingController.view.layoutIfNeeded()
        hostingController.view.snapshot()
        #elseif canImport(AppKit)
        NSHostingView(rootView: self).snapshot()
        #endif
    }
}
#endif
