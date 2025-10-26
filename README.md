# swift-development - emacs

A comprehensive Emacs package for iOS and macOS development with Swift and Xcode.

<picture>
    <img src="https://github.com/konrad1977/swift-development/blob/main/logo/swift-development-logo.png" alt="Swift-development logo" height="300" width="300">
</picture>

## Sponsoring

❤️ [Please sponsor me if you like this package](https://github.com/sponsors/konrad1977)

## Features

### Core Functionality
- **Xcode Integration**: Build, run, and debug iOS apps directly from Emacs
- **Simulator Management**: Control iOS simulators, view logs, and manage devices
- **Auto-Launch Simulator**: Automatically starts simulator when opening a project
- **Multi-Simulator Support**: Run apps on multiple simulators simultaneously
- **Smart Caching**: Automatic build cache warming for faster compilation
- **Ultra-Fast Rebuild Detection**: Last-modified file detection (10-50x faster than hash-based)
- **Persistent Settings**: Project settings survive Emacs restarts
- **LSP Support**: Enhanced Swift language server integration (swift-mode & swift-ts-mode)
- **Project Management**: Automatic scheme detection and project configuration
- **Error Handling**: Advanced error parsing and navigation
- **Flexible Notifications**: Choose between mode-line-hud, minibuffer, or custom notifications

### Developer Tools
- **Refactoring**: Code refactoring utilities for Swift
- **Documentation**: Query Apple Developer Documentation and Hacking with Swift
- **Localization**: Major mode for editing `.strings` files
- **Device Management**: Deploy and debug on physical iOS devices

## Installation

### Prerequisites

- Emacs 27.1 or higher
- Xcode Command Line Tools
- Swift toolchain

### Required Emacs Packages

```elisp
;; Install via package.el or straight.el
(require 'project)
(require 'json)
(require 'request)
```

### Optional Dependencies

- `periphery` - For enhanced error parsing
- `nerd-icons` - For prettier UI elements
- `dape` - For debugging support

### Setup

1. Clone or download this repository to your Emacs load path:

```bash
git clone <repository-url> ~/.emacs.d/localpackages/swift
```

2. Add to your init.el:

```elisp
(add-to-list 'load-path "~/.emacs.d/localpackages/swift")

;; Load the main package
(require 'swift-development)
(require 'xcode-project)
(require 'xcode-build-config)

;; Optional modules
(require 'ios-simulator)
(require 'ios-device)
(require 'swift-refactor)
(require 'localizeable-mode)
```

## File Structure

```
swift/
├── README.md                      # This file
├── swift-development.el           # Main Swift development utilities
├── xcode-project.el               # Xcode project management
├── swift-project-settings.el      # Persistent project settings
├── xcode-build-config.el          # Build configuration and flags
├── swift-cache.el                 # Unified caching system
├── swift-project.el               # Project utilities
├── swift-features.el              # Additional Swift features
├── swift-error-handler.el         # Error parsing and handling
├── swift-lsp.el                   # LSP integration
├── swift-refactor.el              # Refactoring tools
├── xcode-build.el                 # Build system
├── xcodebuildserver.el            # Build server configuration
├── ios-simulator.el               # iOS Simulator integration
├── ios-device.el                  # Physical device management
├── localizeable-mode.el           # Localization file editing
├── apple-docs-query.el            # Apple documentation lookup
└── hacking-with-swift.el          # Hacking with Swift integration
```

## Module Overview

### swift-development.el
Main entry point with build orchestration, app running, and cache warming.

**Key functions:**
- `swift-development-compile-app` - Build the current project
- `swift-development-run-app` - Run app in simulator
- `swift-development-warm-build-cache` - Precompile system frameworks
- `swift-development-toggle-analysis-mode` - Adjust compilation performance

### xcode-project.el
Xcode project and scheme management, build folders, and debugging.

**Key functions:**
- `xcode-project-reset` - Reset project configuration
- `xcode-project-clean-build-folder` - Clean build artifacts
- `xcode-project-cache-diagnostics` - View cache status
- `xcode-project-toggle-device-choice` - Switch between simulator/device
- `xcode-project-start-debugging` - Launch debugger (requires dape)

### swift-project-settings.el
Persistent project settings that survive Emacs restarts. All settings are automatically saved and restored when you reopen your project.

**Key functions:**
- `swift-project-settings-save` - Save project settings to disk
- `swift-project-settings-load` - Load settings from previous session
- `swift-project-settings-show-diagnostics` - View current project settings
- `swift-project-settings-clear` - Clear saved settings
- `swift-project-settings-clear-all-cache` - Clear all cache files
- `ios-simulator-choose-simulator` - Select and save simulator choice

**What it saves:**
- Selected scheme and build configuration
- Simulator selection (device name and ID)
- Device platform (simulator or physical device)
- App identifier and build folder
- Last modified file (for ultra-fast rebuild detection)
- Build configuration (Debug, Release, etc.)
- Last updated timestamp

**Storage location:**
Settings are stored in `.swift-development/` directory in your project root:
- `.swift-development/settings` - Project configuration (~500 bytes)
- `.swift-development/device-cache` - Cached simulator devices (optional, created on simulator selection)

**Auto-launch simulator:**
When `swift-development-auto-launch-simulator` is `t` (default), the simulator automatically starts when you open a project with saved settings. Disable with:
```elisp
(setq swift-development-auto-launch-simulator nil)
```

### xcode-build-config.el
Build configuration, command construction, and optimization flags.

**Key functions:**
- `xcode-build-config-build-app-command` - Generate xcodebuild command
- `xcode-build-config-setup-build-environment` - Configure environment vars
- `xcode-build-config-generate-fast-build-xcconfig` - Create optimized xcconfig

**Key variables:**
- `xcode-build-config-other-swift-flags` - Custom Swift compiler flags
- `xcode-build-config-default-configuration` - Default build configuration
- `xcode-build-config-skip-package-resolution` - Package resolution strategy
- `xcode-build-config-parallel-jobs-multiplier` - CPU cores multiplier for parallel jobs (default: 2)
- `xcode-build-config-link-jobs-divisor` - Divisor for link jobs to reduce memory usage (default: 2)
- `xcode-build-config-swift-exec-memlimit` - Memory limit in MB for Swift compiler (default: 8192)
- `xcode-build-config-xcode-cache-dir` - Xcode DerivedData cache directory
- `xcode-build-config-swift-cache-dir` - Swift Package Manager cache directory
- `xcode-build-config-package-cache-path` - Swift package cache path
- `xcode-build-config-cloned-sources-path` - Cloned Swift package sources path

### swift-cache.el
High-performance caching system for expensive operations.

**Key functions:**
- `swift-cache-clear` - Clear all cached data
- `swift-cache-stats` - Display cache statistics
- `swift-cache-invalidate-project` - Invalidate project-specific cache

### swift-lsp.el
Language Server Protocol (LSP) integration for Swift with proper iOS simulator support.

**Key functions:**
- `swift-lsp-eglot-server-contact` - Configure eglot for Swift development with UIKit/SwiftUI support
- `ios-simulator-target` - Get the current simulator SDK target triple
- `lsp-arguments` - Generate LSP arguments with proper SDK and target configuration

**Setup with eglot:**
```elisp
(require 'swift-lsp)
(require 'eglot)

;; Configure eglot for Swift
(add-to-list 'eglot-server-programs
             '(swift-ts-mode . swift-lsp-eglot-server-contact))
```

The LSP configuration automatically:
- Locates `sourcekit-lsp` via `xcrun`
- Configures the iOS simulator SDK path
- Sets up the correct target triple (e.g., `arm64-apple-ios17.0-simulator`)
- Adds necessary compiler flags for UIKit/SwiftUI development

### xcodebuildserver.el
Automatic Build Server Protocol (BSP) configuration for LSP integration.

**Key functions:**
- `xcodebuildserver-check-configuration` - Verify and generate BSP configuration
- `xcodebuildserver-does-configuration-file-exist` - Check for existing `buildServer.json`

**What it does:**
The package automatically configures the Build Server Protocol for your Xcode project by generating a `buildServer.json` file. This enables advanced LSP features like:
- Accurate code completion for your project's dependencies
- Jump to definition across Swift Package dependencies
- Proper symbol resolution for CocoaPods and Carthage dependencies

**Requirements:**
Install `xcode-build-server` via Homebrew:
```bash
brew install xcode-build-server
```

**Integration:**
The package automatically runs `xcode-build-server config` when you open a project, creating the necessary configuration file. After building your project, the build output is parsed and fed to `xcode-build-server parse` to keep the LSP server synchronized with your build state.

### ios-simulator.el
iOS Simulator control and log viewing.

**Key functions:**
- `ios-simulator:run` - Launch app in simulator
- `ios-simulator:view-logs` - View simulator logs
- `ios-simulator:reset` - Reset simulator state

### ios-device.el
Physical device deployment and debugging.

**Key functions:**
- `ios-device:choose-device` - Select a connected device
- `ios-device:reset-privacy` - Reset app privacy settings on device

### swift-refactor.el
Code refactoring utilities.

**Key functions:**
- `swift-refactor:extract-function` - Extract code to new function
- `swift-refactor:rename-symbol` - Rename symbols across project

### localizeable-mode.el
Major mode for editing .strings localization files with syntax highlighting.

### apple-docs-query.el / hacking-with-swift.el
Quick documentation lookup from Emacs.

**Key functions:**
- `apple-docs/query` - Search Apple Developer Documentation
- `hacking-ws/query` - Search Hacking with Swift tutorials

## Usage Examples

### Building and Running

```elisp
;; Build the current project
M-x swift-development:compile-app

;; Run in simulator
M-x swift-development:run-app

;; Build and run in one command
M-x swift-development:build-and-run
```

### Cache Management

The package automatically warms the build cache when you open a Swift file in a new project. This precompiles system frameworks (Foundation, UIKit, SwiftUI, etc.) to speed up subsequent builds.

```elisp
;; View cache diagnostics
M-x xcode-project:cache-diagnostics

;; Manually warm cache
M-x swift-development:warm-build-cache

;; Clear all caches
M-x swift-cache-clear
```

### Device and Simulator Management

```elisp
;; Choose simulator (automatically saved to project settings)
M-x ios-simulator-choose-simulator

;; Switch between simulator and device
M-x xcode-project:toggle-device-choice

;; View simulator logs
M-x ios-simulator:view-logs

;; Reset simulator
M-x ios-simulator:reset

;; Invalidate simulator device cache (forces refresh)
M-x ios-simulator-invalidate-cache
```

**Auto-launch feature:**
After selecting a simulator once, it will automatically launch when you reopen the project (if `swift-development-auto-launch-simulator` is `t`, which is the default).

### Documentation Lookup

```elisp
;; Search Apple Docs for symbol at point
M-x apple-docs/query-thing-at-point

;; Search Hacking with Swift
M-x hacking-ws/query
```

## Notification System

The package includes a flexible notification system that can display build progress, errors, and status updates through different backends.

### Notification Backends

```elisp
;; Choose your preferred notification backend
(setq xcode-project-notification-backend 'mode-line-hud)  ; Default: show in mode-line
;; (setq xcode-project-notification-backend 'message)     ; Alternative: use minibuffer messages
;; (setq xcode-project-notification-backend 'custom)      ; Use custom function
```

**Available backends:**
- `mode-line-hud` - Display notifications in the mode-line using mode-line-hud (recommended)
- `message` - Display notifications in the minibuffer
- `custom` - Use a custom notification function (set via `xcode-project-notification-function`)

All notifications automatically force a display update before blocking operations, ensuring you always see status messages before long-running tasks.

## Ultra-Fast Rebuild Detection

The package automatically detects when rebuilds are needed using an extremely fast last-modified file detection system (0.1-0.5 seconds for 1000+ files).

### How It Works

Instead of hashing all files, the system uses a single `find` command to locate the most recently modified source file:

1. **Single Command**: One `find | stat | sort | head` command finds the most recently modified file
2. **Timestamp Comparison**: Compares timestamp + filepath with saved value in settings
3. **Ultra-Fast**: 10-50x faster than hash-based detection (0.1-0.5s vs 2-5s)
4. **Persistent**: Saved in `.swift-development/settings`, survives Emacs restarts
5. **Smart Skipping**: Skips rebuild if no watched files changed since last successful build

**What's monitored:**
- Swift, Obj-C, C/C++ source files
- UI resources (Storyboards, XIBs, Asset Catalogs)
- Test files are ignored by default (configurable)

### Configuration

```elisp
;; Customize which files trigger rebuilds
(setq swift-development-watched-extensions
      '("swift" "m" "mm" "h" "c" "cpp" "storyboard" "xib" "xcassets"))

;; Customize ignored paths (tests don't affect app bundle)
(setq swift-development-ignore-paths
      '("*Tests/*" "*/Tests.swift" "*UITests/*"))

;; Example: Ignore additional paths
(setq swift-development-ignore-paths
      '("*Tests/*" "*/Tests.swift" "*UITests/*" "*Pods/*" "*Generated/*"))
```

### Force Rebuild

Sometimes you need to rebuild regardless of file changes:

```elisp
;; Force rebuild next time
M-x swift-development-reset-build-status

;; Clear all cache files (includes last-modified data)
M-x swift-development-clear-hash-cache

;; Full project reset (clears all settings and caches)
M-x xcode-project-reset
```

### Performance Comparison

```
Old System (hash-based):
- Scan ~1056 files
- Start 1056 async MD5 processes
- Wait for callbacks
- Time: 2-5 seconds

New System (last-modified):
- One find command
- Compare timestamp + path
- Time: 0.1-0.5 seconds

Result: 10-50x faster! 🚀
```

## Multi-Simulator Support

Run your app on multiple simulators simultaneously for testing across different devices/iOS versions.

### Setup Target Simulators

```elisp
;; Add simulators to target list
M-x ios-simulator-add-target-simulator

;; List all target simulators
M-x ios-simulator-list-target-simulators

;; Remove a simulator from targets
M-x ios-simulator-remove-target-simulator

;; Clear all targets (back to single simulator mode)
M-x ios-simulator-clear-target-simulators
```

### Running on Multiple Simulators

Once configured, `M-x swift-development:run-app` will automatically launch your app on all target simulators.

### Ad-hoc Multi-Simulator Testing

```elisp
;; Run on an additional simulator without changing targets
M-x ios-simulator-run-on-additional-simulator

;; List all active simulators with running apps
M-x ios-simulator-list-active-simulators

;; Terminate app on specific simulator
M-x ios-simulator-terminate-app-on-simulator

;; Shutdown a specific simulator
M-x ios-simulator-shutdown-simulator
```

## Configuration

### Swift Mode Support

The package fully supports both `swift-mode` and `swift-ts-mode` (tree-sitter). Auto-warming, working directory setup, and all hooks work identically with both modes.

```elisp
;; Works automatically with:
;; - swift-mode (traditional mode)
;; - swift-ts-mode (tree-sitter mode, recommended)
```

### Custom Variables

```elisp
;; Enable debug mode for troubleshooting
(setq xcode-project-debug t)
(setq swift-development-debug t)
(setq swift-project-settings-debug t)
(setq ios-simulator-debug t)

;; Set cache TTL (default: 300 seconds)
(setq swift-cache-ttl 600)

;; Disable auto-launch simulator (enabled by default)
(setq swift-development-auto-launch-simulator nil)

;; Customize build ignore list
(setq xcode-project-clean-build-ignore-list '("ModuleCache.noindex" "SourcePackages"))

;; Enable cache debug logging
(setq swift-cache-debug t)
```

### Rebuild Detection Configuration

Control which files trigger rebuilds when modified:

```elisp
;; File extensions to watch for changes (default includes source code and UI files)
(setq swift-development-watched-extensions
      '("swift" "m" "mm" "h" "c" "cpp" "storyboard" "xib" "xcassets"))

;; Path patterns to ignore when checking for rebuilds
;; Test files are ignored by default since they don't affect the app bundle
(setq swift-development-ignore-paths
      '("*Tests/*" "*/Tests.swift" "*UITests/*"))

;; Example: Also ignore CocoaPods and generated files
(setq swift-development-ignore-paths
      '("*Tests/*" "*/Tests.swift" "*UITests/*" "*Pods/*" "*Generated/*"))

;; Example: Only watch Swift files (fastest, but ignores Obj-C and resources)
(setq swift-development-watched-extensions '("swift"))
```

The rebuild detection system checks modification times to avoid unnecessary builds. By default:
- **Watched**: Swift, Obj-C, C/C++, Storyboards, XIBs, and Asset Catalogs
- **Ignored**: Test files (they don't affect the app bundle)
- **Also ignored**: Hidden files/folders (`.git`, `.build`) and `DerivedData`

### Build Configuration

```elisp
;; Adjust parallel jobs multiplier (higher = faster but more memory)
(setq xcode-build-config-parallel-jobs-multiplier 3)  ; Default: 2

;; Reduce link jobs to save memory on machines with limited RAM
(setq xcode-build-config-link-jobs-divisor 4)  ; Default: 2

;; Increase Swift compiler memory limit for very large files
(setq xcode-build-config-swift-exec-memlimit 16384)  ; Default: 8192 (8GB)

;; Custom Swift compiler flags
(setq xcode-build-config-other-swift-flags
      '("-no-whole-module-optimization"
        "-enable-actor-data-race-checks"))

;; Change default build configuration
(setq xcode-build-config-default-configuration "Release")

;; Package resolution strategy
(setq xcode-build-config-skip-package-resolution 'always)  ; 'auto, 'always, or 'never

;; Custom cache directories (useful for network drives or alternative locations)
(setq xcode-build-config-xcode-cache-dir "/Volumes/Build/DerivedData")
(setq xcode-build-config-swift-cache-dir "/Volumes/Build/SwiftCache")
```

### Recommended Key Bindings

```elisp
(with-eval-after-load 'swift-mode
  (define-key swift-mode-map (kbd "C-c C-c") 'swift-development:compile-app)
  (define-key swift-mode-map (kbd "C-c C-r") 'swift-development:run-app)
  (define-key swift-mode-map (kbd "C-c C-d") 'xcode-project:start-debugging)
  (define-key swift-mode-map (kbd "C-c C-k") 'xcode-project:reset)
  (define-key swift-mode-map (kbd "C-c C-l") 'ios-simulator:view-logs))
```

## Cache System

The package uses a two-tier caching system:

### 1. Swift Cache (Emacs-level)
Caches expensive operations like:
- Build settings (TTL: 30 minutes)
- Scheme files (TTL: 10 minutes)
- Build folder locations (TTL: 30 minutes)

### 2. Build Cache Warming (Xcode-level)
Precompiles system frameworks on first project open:
- Foundation, UIKit, SwiftUI, Combine, CoreData, CoreGraphics
- Bridging headers (.pch files)
- Stored in `~/Library/Caches/` and `~/Library/Developer/Xcode/DerivedData/ModuleCache`

## Troubleshooting

### Build Issues

```elisp
;; Check build folder detection
M-x xcode-project:debug-build-folder-detection

;; View current configuration
M-x xcode-project:show-current-configuration

;; Reset everything and start fresh
M-x xcode-project:reset
```

### Cache Issues

```elisp
;; View cache diagnostics
M-x xcode-project:cache-diagnostics

;; View project settings diagnostics
M-x swift-project-settings-show-diagnostics

;; Diagnose auto-warming issues (run from Swift buffer)
M-x swift-development-diagnose-auto-warm

;; Test auto-warming manually
M-x swift-development-test-auto-warm

;; Clear build folder cache
M-x xcode-project:clear-build-folder-cache

;; Clear all cache files (settings, device-cache, last-modified)
M-x swift-development-clear-hash-cache

;; Clear all caches
M-x swift-cache-clear

;; Invalidate simulator device cache
M-x ios-simulator-invalidate-cache

;; View cache statistics
M-x swift-cache-stats
```

### Build Status and Monitoring

```elisp
;; Check if rebuild is needed
M-x swift-development-build-status

;; View detailed build process status
M-x xcode-project:build-status

;; Show last build errors
M-x swift-development-show-last-build-errors

;; Show full build output
M-x swift-development-show-build-output

;; Hide build output buffer
M-x swift-development-hide-build-output
```

### Interrupt Stuck Builds

```elisp
;; Interrupt current build
M-x xcode-project:interrupt-build

;; Kill all xcodebuild processes
M-x xcode-project:kill-all-xcodebuild-processes

;; Reset build status (clears "already built" state)
M-x swift-development-reset-build-status

;; Check for compile.lock errors
M-x xcode-project:check-compile-lock-error
```

## Performance Tips

1. **Use Fast Mode**: `M-x swift-development:set-fast-mode` for balanced performance
2. **Let Cache Warm**: Don't interrupt the initial cache warming process
3. **Clean Periodically**: Run `M-x xcode-project:clean-build-folder` when switching branches
4. **Monitor Cache**: Check `M-x xcode-project:cache-diagnostics` if builds feel slow

## Known Issues

- Cache warming is project-specific and runs once per Emacs session
- Physical device deployment requires proper code signing setup
- Some Xcode features (Storyboards, Asset Catalogs) work better in Xcode.app

## Contributing

This is a personal Emacs configuration package. Feel free to fork and adapt for your needs.

## License

MIT

## Credits

Developed for efficient iOS/macOS development in Emacs.

## Changelog

### Recent Updates (2025-10-26)

#### 🚀 Ultra-Fast Rebuild Detection
- **Replaced hash-based system with last-modified detection**
  - 10-50x faster rebuild checks (0.1-0.5s vs 2-5s)
  - Single `find | stat | sort | head` command instead of 1000+ MD5 processes
  - Persisted in `.swift-development/settings` for cross-session consistency
  - Automatically updated after each successful build

#### 🎯 Auto-Launch Simulator
- **Automatic simulator startup on project open**
  - Enabled by default via `swift-development-auto-launch-simulator`
  - Starts saved simulator automatically when opening project with settings
  - No more manual simulator selection after first setup
  - Disable with `(setq swift-development-auto-launch-simulator nil)`

#### 💾 Enhanced Persistent Settings
- **Comprehensive project state preservation**
  - Simulator selection (device name and ID)
  - Build configuration (Debug, Release, etc.)
  - App identifier and build folder
  - Last modified file for rebuild detection
  - Platform choice (simulator vs device)
  - All settings survive Emacs restarts

#### ⚡ Simulator Device Cache
- **Fast simulator selection**
  - Device list cached in `.swift-development/device-cache`
  - Validated against current scheme and project
  - Automatically invalidated when context changes
  - Manual refresh with `ios-simulator-invalidate-cache`
  - Interactive simulator selection with `ios-simulator-choose-simulator`

#### 🧹 Improved Reset Functionality
- **Complete project reset**
  - `xcode-project-reset` now clears ALL cache files
  - Removes settings, device-cache, and last-modified data
  - Clean slate for troubleshooting or project switching
  - Hash-based file-cache system removed (no longer needed)

#### 📝 Better Settings Management
- **Automatic saving on all relevant operations**
  - App identifier saved after first fetch
  - Build configuration saved after determination
  - Build folder saved after detection
  - Settings captured throughout the build process
  - No manual intervention required

### Performance Impact

```
Build Check Performance:
Before: 2-5 seconds (hash-based)
After:  0.1-0.5 seconds (last-modified)
Improvement: 10-50x faster ⚡

Disk Usage:
Before: ~153 KB (file-cache with hashes)
After:  ~500 bytes (last-modified in settings)
Savings: 99.7% reduction in cache file size

Code Complexity:
Removed: ~300 lines of hash-related code
Added:   ~50 lines of last-modified detection
Net:     Much simpler and more maintainable
```

See git history for complete changes.

## Swift Development Mode

The package includes `swift-development-mode`, a minor mode that provides a unified keymap across all Swift-related buffers (Swift files, .strings files, and iOS simulator output buffers).

### Activation

The mode automatically activates for:
- Swift source files (`swift-ts-mode`)
- Localizeable .strings files (`localizeable-mode`)
- iOS simulator output buffers

### Key Bindings

#### Build & Run
- `C-c C-c` - Compile and run app
- `C-c C-b` - Compile app only
- `M-r` - Run last built app
- `C-c C-x` - Reset build state
- `C-c b s` - Show build status

#### Testing
- `C-c t m` - Run tests for current module
- `C-c t p` - Run Swift package tests

#### Simulator Control
- `M-s` - Terminate current app in simulator
- `C-x s n` - Send push notification to simulator
- `C-x s t` - Toggle simulator output buffer
- `C-x s l` - Change simulator language

#### Xcode Integration
- `M-K` - Clean build folder
- `C-c C-d` - Start debugging
- `C-c x t` - Toggle device/simulator
- `C-c x c` - Show current configuration

#### Refactoring
- `M-t` - Insert TODO comment
- `M-m` - Insert MARK comment
- `C-c r a` - Wrap selection in block
- `C-c r d` - Delete matching braces
- `C-c r i` - Tidy up constructor
- `C-c r r` - Extract function
- `M-P` - Print debug statement
- `C-c r t` - Add try-catch
- `C-c r s` - Split function parameters

#### Code Navigation
- `C-x p t` - Toggle periphery buffer
- `C-c C-f` - Search with ripgrep

### Manual Control

You can manually toggle the mode in any buffer:

```elisp
M-x swift-development-mode
```

