#if canImport(SwiftUI)
import SwiftUI

extension View {
    @ViewBuilder
    public func setupSwiftDevelopmentPreview<Content: View>(
        hotReload: Bool = false,
        @ViewBuilder _ content: @escaping () -> Content
    ) -> some View {
        if SwiftDevelopmentPreview.isInPreview {
            let view = self
                .onAppear {
                    // Only initialize once
                    guard !SwiftDevelopmentPreview.hotReloadInitialized else { return }
                    SwiftDevelopmentPreview.markHotReloadInitialized()

                    // Set hot reload flag globally
                    SwiftDevelopmentPreview.isHotReloadEnabled = hotReload

                    // Initial snapshot after layout
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                        content().snapshot()
                    }

                    // Setup hot reload observer if enabled
                    if hotReload {
                        #if canImport(Combine)
                        HotReloadObserver.shared.startObserving {
                            // Wait a bit for Inject to finish injecting code
                            DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
                                print("📸 Taking new snapshot after hot reload...")
                                content().snapshot()
                            }
                        }
                        print("🔥 Hot reload observer started")
                        #else
                        print("⚠️ Hot reload requires Combine framework")
                        #endif
                    }
                }
                .onDisappear {
                    if hotReload && SwiftDevelopmentPreview.hotReloadInitialized {
                        #if canImport(Combine)
                        HotReloadObserver.shared.stopObserving()
                        SwiftDevelopmentPreview.resetHotReloadInitialization()
                        print("🔥 Hot reload observer stopped")
                        #endif
                    }
                }

            // Apply enableInjection if hot reload is enabled
            if hotReload {
                view.enableInjection()
            } else {
                view
            }
        } else {
            self
        }
    }
}
#endif
