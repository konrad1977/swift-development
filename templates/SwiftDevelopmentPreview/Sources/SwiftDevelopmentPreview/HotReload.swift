#if canImport(Combine)
import Combine
import Foundation

/// Observer for hot reload events from Inject/InjectionIII
public class HotReloadObserver {
    private var cancellable: AnyCancellable?

    public static let shared = HotReloadObserver()

    private init() {}

    /// Start observing for injection notifications
    /// - Parameter onReload: Closure to execute when hot reload occurs
    public func startObserving(onReload: @escaping () -> Void) {
        cancellable = NotificationCenter.default
            .publisher(for: .init("INJECTION_BUNDLE_NOTIFICATION"))
            .sink { _ in
                print("🔥 Hot reload triggered - generating new preview")
                onReload()
            }
    }

    /// Stop observing injection notifications
    public func stopObserving() {
        cancellable?.cancel()
        cancellable = nil
    }
}
#endif
