#if canImport(SwiftUI)
import SwiftUI

extension SwiftDevelopmentPreview {
    public static func setup(view: any View, named viewName: String? = nil) {
        guard isInPreview else { return }

        // If a specific view name is requested, only snapshot if it matches
        if let requestedViewName = previewViewName {
            // If viewName is provided, check if it matches
            if let currentViewName = viewName {
                guard currentViewName == requestedViewName else {
                    return
                }
            } else {
                // If no viewName provided in setup, try to infer from type
                let typeName = String(describing: type(of: view))
                // Extract just the type name without module prefix if present
                let actualViewName = typeName.components(separatedBy: ".").last ?? typeName
                guard actualViewName == requestedViewName else {
                    return
                }
            }
        }

        view.snapshot()
    }
}
#endif
