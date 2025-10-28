#if canImport(SwiftUI)
import SwiftUI

extension View {
    func snapshot() {
        #if canImport(UIKit)
        let hostingController = UIHostingController(rootView: self)
        // Set a reasonable size for the hosting controller
        hostingController.view.frame = CGRect(x: 0, y: 0, width: 393, height: 852)
        hostingController.view.layoutIfNeeded()
        hostingController.view.snapshot()
        #elseif canImport(AppKit)
        NSHostingView(rootView: self).snapshot()
        #endif
    }
}
#endif
