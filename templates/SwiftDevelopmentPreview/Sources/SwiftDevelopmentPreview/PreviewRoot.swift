import Foundation
import SwiftUI

/// Root view that dynamically displays the requested preview view
public struct PreviewRoot<Content: View>: View {
    let fallbackContent: Content

    public init(@ViewBuilder content: () -> Content) {
        self.fallbackContent = content()
    }

    public var body: some View {
        if SwiftDevelopmentPreview.isInPreview,
           let requestedViewName = SwiftDevelopmentPreview.previewViewName,
           let view = ViewRegistry.shared.view(for: requestedViewName) {
            view
        } else {
            fallbackContent
        }
    }
}
