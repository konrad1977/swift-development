# swift-emacs

A comprehensive Emacs package for iOS and macOS development with Swift and Xcode.

## Features

### Core Functionality
- **Xcode Integration**: Build, run, and debug iOS apps directly from Emacs
- **Simulator Management**: Control iOS simulators, view logs, and manage devices
- **Smart Caching**: Automatic build cache warming for faster compilation
- **LSP Support**: Enhanced Swift language server integration
- **Project Management**: Automatic scheme detection and project configuration
- **Error Handling**: Advanced error parsing and navigation

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
;; Switch between simulator and device
M-x xcode-project:toggle-device-choice

;; View simulator logs
M-x ios-simulator:view-logs

;; Reset simulator
M-x ios-simulator:reset
```

### Documentation Lookup

```elisp
;; Search Apple Docs for symbol at point
M-x apple-docs/query-thing-at-point

;; Search Hacking with Swift
M-x hacking-ws/query
```

## Configuration

### Custom Variables

```elisp
;; Enable debug mode for troubleshooting
(setq xcode-project-debug t)

;; Set cache TTL (default: 300 seconds)
(setq swift-cache-ttl 600)

;; Customize build ignore list
(setq xcode-project-clean-build-ignore-list '("ModuleCache.noindex" "SourcePackages"))

;; Enable cache debug logging
(setq swift-cache-debug t)
```

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

;; Clear build folder cache
M-x xcode-project:clear-build-folder-cache

;; Clear all caches
M-x swift-cache-clear
```

### Interrupt Stuck Builds

```elisp
;; Interrupt current build
M-x xcode-project:interrupt-build

;; Check build status
M-x xcode-project:build-status

;; Kill all xcodebuild processes
M-x xcode-project:kill-all-xcodebuild-processes
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

(Add your license here)

## Credits

Developed for efficient iOS/macOS development in Emacs.

## Changelog

See git history for changes.

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

