import Foundation
import SwiftUI

/// Registry for dynamically selecting views at runtime
public class ViewRegistry {
    public static let shared = ViewRegistry()

    private var views: [String: () -> AnyView] = [:]

    private init() {}

    /// Register a view with a name
    public func register<V: View>(name: String, _ builder: @escaping () -> V) {
        views[name] = { AnyView(builder()) }
    }

    /// Get a view by name
    public func view(for name: String) -> AnyView? {
        return views[name]?()
    }

    /// Get all registered view names
    public var registeredNames: [String] {
        return Array(views.keys)
    }
}
