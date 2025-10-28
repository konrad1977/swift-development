# SwiftDevelopmentPreview

A Swift package that enables SwiftUI preview generation for Emacs swift-development.el.

## Features

- Capture SwiftUI view snapshots from your running iOS/macOS app
- Minimal integration - just add a single modifier to your views
- Supports iOS, macOS, tvOS, watchOS, and visionOS
- No external dependencies

## Installation

### 1. Copy Package to Your Project

Copy this entire `SwiftDevelopmentPreview` directory to your project root or a packages subdirectory.

### 2. Add as Local Package Dependency

In Xcode:
1. File → Add Package Dependencies...
2. Click "Add Local..."
3. Select the `SwiftDevelopmentPreview` directory
4. Add to your app target

Or add to your `Package.swift` if you're using SPM:

```swift
dependencies: [
    .package(path: "./SwiftDevelopmentPreview")
]
```

### 3. Import in Your Views

```swift
import SwiftDevelopmentPreview
```

## Usage

### SwiftUI Views

Add the `.setupSwiftDevelopmentPreview()` modifier to any SwiftUI view you want to preview:

```swift
import SwiftUI
import SwiftDevelopmentPreview

struct ContentView: View {
    var body: some View {
        VStack {
            Text("Hello, World!")
            Button("Tap me") {
                print("Tapped")
            }
        }
        .setupSwiftDevelopmentPreview { self }
    }
}
```

The closure parameter allows you to specify which view to capture. You can capture the current view with `self`, or any other view:

```swift
.setupSwiftDevelopmentPreview {
    MyDetailView(item: sampleItem)
}
```

### UIKit Views

For UIKit views, use the static setup method:

```swift
import UIKit
import SwiftDevelopmentPreview

class MyViewController: UIViewController {
    override func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
        SwiftDevelopmentPreview.setup(view: self.view)
    }
}
```

### AppKit Views (macOS)

For AppKit views:

```swift
import AppKit
import SwiftDevelopmentPreview

class MyView: NSView {
    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        SwiftDevelopmentPreview.setup(view: self)
    }
}
```

## How It Works

1. When your app is launched with the `--swift-development-preview` flag, the package detects it
2. After a 0.5-second delay (to allow layout to complete), a snapshot is captured
3. The snapshot is saved as a PNG to the path specified by `--preview-path` argument
4. The app automatically exits after saving the preview

The Emacs integration handles launching your app with the correct flags and displaying the generated preview.

## Preview Path

The package reads the output path from command-line arguments:
- `--preview-path=/path/to/output.png` - Specifies where to save the preview
- Falls back to `/tmp/swift-development-preview-{bundle-name}.png` if not specified

## Hot Reload (Optional)

For instant preview updates without rebuilding, the package includes hot reload support via [Inject](https://github.com/krzysztofzablocki/Inject) (automatically included as a dependency with required build settings).

### Setup

1. **Install InjectionIII** (one-time per machine):
   ```bash
   curl -L -o InjectionIII.app.zip https://github.com/johnno1962/InjectionIII/releases/latest/download/InjectionIII.app.zip
   unzip InjectionIII.app.zip && mv InjectionIII.app /Applications/
   ```

2. **Update your view**:
   ```swift
   import SwiftUI
   import SwiftDevelopmentPreview

   struct ContentView: View {
       var body: some View {
           VStack {
               Text("Hello, World!")
           }
           .setupSwiftDevelopmentPreview(hotReload: true) { self }
       }
   }
   ```

3. **Start InjectionIII**:
   ```bash
   open /Applications/InjectionIII.app
   # Select your .xcworkspace in the File Watcher
   ```

4. **Use hot reload in Emacs**:
   ```elisp
   ;; First time: C-c p h (generate with hot reload)
   ;; Then: Edit code → Save → Preview updates in ~1-2s!
   ;; Stop: C-c p s (stop hot reload)
   ```

### How It Works

With hot reload:
- **First run**: Normal build + app launch
- **Code changes**: Inject injects new code → app takes new snapshot automatically
- **No rebuild** needed between changes!

Without hot reload:
- Each change requires full rebuild (~10-30s)

## Troubleshooting

### Preview not generating

1. Make sure you've added `.setupSwiftDevelopmentPreview()` to your view
2. Check that the view is actually being displayed (use `onAppear` debug print)
3. Verify the app is receiving the `--swift-development-preview` flag
4. Check console output for error messages

### Preview is blank or incorrect

1. Try increasing the delay in `View+setup.swift` (default is 0.5 seconds)
2. Make sure your view has a non-zero size
3. For complex views, ensure all data is loaded before snapshot

### Hot reload not working

1. Verify InjectionIII is running and watching your project
2. Verify `hotReload: true` is set in `.setupSwiftDevelopmentPreview()`
3. Check console for "🔥 Hot reload observer started" messages
4. Check console for "📸 Taking new snapshot after hot reload..." when you save changes

### Build errors

1. Make sure you're targeting iOS 14+ / macOS 11+
2. Check that SwiftUI is imported in your files
3. Clean build folder and rebuild

## Technical Details

The package consists of:
- `SwiftDevelopmentPreview.swift` - Flag and path detection
- `HotReload.swift` - Hot reload observer using Combine
- `View+setup.swift` - SwiftUI modifier for preview setup
- `View+snapshot.swift` - Platform-agnostic snapshot interface
- `UIView+snapshot.swift` - iOS/tvOS/watchOS snapshot implementation
- `NSView+snapshot.swift` - macOS snapshot implementation
- Platform-specific setup helpers for UIKit and AppKit

## License

This package is part of swift-development.el and follows the same license.
