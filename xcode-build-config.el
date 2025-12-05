;;; xcode-build-config.el --- Xcode build configuration and command construction -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.5.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, xcode, build

;;; Commentary:

;; This module handles all build configuration, command construction,
;; environment setup, and build flags for Xcode builds.
;; Extracted from swift-development.el for better modularity.

;;; Code:

;; Optional dependencies
(require 'swift-cache nil t)

;; Forward declarations to avoid circular dependency
(declare-function xcode-project-project-root "xcode-project")
(declare-function xcode-project-get-workspace-or-project "xcode-project")
(declare-function xcode-project-scheme "xcode-project")

;;; ============================================================================
;;; Customization Variables
;;; ============================================================================

(defgroup xcode-build-config nil
  "Xcode build configuration and optimization settings."
  :group 'programming
  :prefix "xcode-build-config-")

(defcustom xcode-build-config-default-configuration nil
  "Default build configuration to use. If nil, use scheme's default."
  :type '(choice (const :tag "Use scheme's default" nil)
                 (string :tag "Configuration name"))
  :group 'xcode-build-config)

(defcustom xcode-build-config-modern-build-system "YES"
  "Whether to use the modern build system."
  :type '(choice (const "YES")
                 (const "NO"))
  :group 'xcode-build-config)

(defcustom xcode-build-config-additional-build-flags '()
  "Additional flags to pass to xcodebuild."
  :type '(repeat string)
  :group 'xcode-build-config)

(defcustom xcode-build-config-other-swift-flags '("-no-whole-module-optimization")
  "Additional flags to pass to the Swift compiler (OTHER_SWIFT_FLAGS)."
  :type '(repeat string)
  :group 'xcode-build-config)

(defcustom xcode-build-config-other-ld-flags '()
  "Additional flags to pass to the linker (OTHER_LDFLAGS).
For hot reload with Inject/InjectionIII, set to '(\"-Xlinker\" \"-interposable\")."
  :type '(repeat string)
  :group 'xcode-build-config)

(defcustom xcode-build-config-enable-timing-summary t
  "Enable build timing summary to identify bottlenecks."
  :type 'boolean
  :group 'xcode-build-config)

(defcustom xcode-build-config-use-thin-lto nil
  "Use Thin LTO for faster linking (experimental)."
  :type 'boolean
  :group 'xcode-build-config)

(defcustom xcode-build-config-skip-package-resolution 'auto
  "Control Swift package dependency resolution during builds.
- 'auto: Automatically detect if packages need resolution (recommended)
- 'always: Always skip package resolution (faster but may fail if packages missing)
- 'never: Never skip, always resolve packages (slower but safer)"
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "Always skip" always)
                 (const :tag "Never skip" never))
  :group 'xcode-build-config)

(defcustom xcode-build-config-force-package-resolution nil
  "Force package resolution even if packages appear to exist.
Useful when you suspect package corruption or version conflicts."
  :type 'boolean
  :group 'xcode-build-config)

(defcustom xcode-build-config-parallel-jobs-multiplier 2
  "Multiplier for parallel jobs based on CPU cores.
Higher values may speed up builds but increase memory usage."
  :type 'integer
  :group 'xcode-build-config)

(defcustom xcode-build-config-link-jobs-divisor 2
  "Divisor for parallel link jobs (cores / divisor).
Linking is memory-intensive, so using fewer jobs prevents OOM errors."
  :type 'integer
  :group 'xcode-build-config)

(defcustom xcode-build-config-swift-exec-memlimit 8192
  "Memory limit in MB for Swift compiler processes.
Higher values allow compilation of larger files but require more RAM."
  :type 'integer
  :group 'xcode-build-config)

(defcustom xcode-build-config-xcode-cache-dir "~/Library/Developer/Xcode/DerivedData"
  "Base directory for Xcode's DerivedData and caches."
  :type 'directory
  :group 'xcode-build-config)

(defcustom xcode-build-config-swift-cache-dir "~/Library/Caches/org.swift.swiftpm"
  "Base directory for Swift Package Manager caches."
  :type 'directory
  :group 'xcode-build-config)

(defcustom xcode-build-config-package-cache-path "~/Library/Caches/org.swift.packages"
  "Path to Swift package cache directory."
  :type 'directory
  :group 'xcode-build-config)

(defcustom xcode-build-config-cloned-sources-path "~/Library/Caches/org.swift.cloned-sources"
  "Path to cloned Swift package sources directory."
  :type 'directory
  :group 'xcode-build-config)

;;; Build Output Patterns
(defcustom xcode-build-config-build-succeeded-pattern "\\*\\* BUILD SUCCEEDED \\*\\*"
  "Regex pattern to detect successful builds."
  :type 'string
  :group 'xcode-build-config)

(defcustom xcode-build-config-build-failed-pattern "\\*\\* BUILD FAILED \\*\\*"
  "Regex pattern to detect failed builds."
  :type 'string
  :group 'xcode-build-config)

(defcustom xcode-build-config-build-interrupted-pattern "\\*\\* BUILD INTERRUPTED \\*\\*"
  "Regex pattern to detect interrupted builds."
  :type 'string
  :group 'xcode-build-config)

(defcustom xcode-build-config-xcodebuild-error-pattern "^xcodebuild: error:"
  "Regex pattern to detect xcodebuild errors."
  :type 'string
  :group 'xcode-build-config)

(defcustom xcode-build-config-error-keyword-pattern "\\berror:"
  "Regex pattern to detect error keywords in output."
  :type 'string
  :group 'xcode-build-config)

(defcustom xcode-build-config-warning-keyword-pattern "\\bwarning:"
  "Regex pattern to detect warning keywords in output."
  :type 'string
  :group 'xcode-build-config)

(defcustom xcode-build-config-compile-lock-pattern "\\.compile\\.lock.*locked so long"
  "Regex pattern to detect compile lock errors."
  :type 'string
  :group 'xcode-build-config)

(defcustom xcode-build-config-error-location-pattern "^\\(.+:\\([0-9]+\\):\\([0-9]+\\): error:\\|\\*\\* BUILD FAILED \\*\\*\\|xcodebuild: error:\\|The following build commands failed:\\)"
  "Regex pattern to detect error locations in build output.
Captures file path, line number, and column number."
  :type 'string
  :group 'xcode-build-config)

(defcustom xcode-build-config-simple-error-pattern "^\\(error\\|\\*\\* BUILD FAILED \\*\\*\\|xcodebuild: error:\\)"
  "Simple regex pattern to quickly detect errors in build output."
  :type 'string
  :group 'xcode-build-config)

(defvar xcode-build-config-optimization-level 'fast
  "Build optimization level.")

;;; ============================================================================
;;; Internal Variables
;;; ============================================================================

(defvar xcode-build-config--current-environment-x86 nil
  "Whether to use x86_64 architecture.")

(defvar xcode-build-config--current-build-command nil
  "Cached build command for current configuration.")

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun xcode-build-config-get-optimal-jobs ()
  "Get optimal number of parallel jobs based on CPU cores."
  (number-to-string (max 1 (num-processors))))

(defun xcode-build-config-uses-cocoapods-p ()
  "Check if the current project uses CocoaPods."
  (let* ((project-root (xcode-project-project-root))
         (cache-key (when (fboundp 'swift-cache-project-key)
                      (swift-cache-project-key project-root "uses-cocoapods"))))
    (if (and cache-key (fboundp 'swift-cache-with))
        (swift-cache-with cache-key 1800  ; Cache for 30 minutes
          (let ((default-directory project-root))
            (or (file-exists-p "Podfile")
                (file-exists-p "Podfile.lock")
                (file-exists-p "Pods/Pods.xcodeproj"))))
      ;; Fallback without caching
      (let ((default-directory (xcode-project-project-root)))
        (or (file-exists-p "Podfile")
            (file-exists-p "Podfile.lock")
            (file-exists-p "Pods/Pods.xcodeproj"))))))

;;; ============================================================================
;;; Swift Package Management
;;; ============================================================================

(defun xcode-build-config-swift-packages-exist-p ()
  "Check if Swift packages are already downloaded.
Looks for packages in both the local .build folder and global caches."
  (let* ((project-root (xcode-project-project-root))
         (local-packages (expand-file-name ".build/checkouts" project-root))
         (global-packages (expand-file-name xcode-build-config-xcode-cache-dir))
         (cache-packages (expand-file-name xcode-build-config-package-cache-path))
         (cloned-sources (expand-file-name xcode-build-config-cloned-sources-path)))
    (or
     ;; Check local .build/checkouts
     (and (file-exists-p local-packages)
          (> (length (directory-files local-packages nil "^[^.]" t)) 0))
     ;; Check global package caches
     (and (file-exists-p cache-packages)
          (> (length (directory-files cache-packages nil "^[^.]" t)) 0))
     ;; Check cloned sources
     (and (file-exists-p cloned-sources)
          (> (length (directory-files cloned-sources nil "^[^.]" t)) 0)))))

(defun xcode-build-config-should-resolve-packages-p ()
  "Determine if package resolution should be performed.
Based on xcode-build-config-skip-package-resolution setting."
  (cond
   ;; Force resolution if explicitly requested
   (xcode-build-config-force-package-resolution t)
   ;; Check skip setting
   ((eq xcode-build-config-skip-package-resolution 'never) t)
   ((eq xcode-build-config-skip-package-resolution 'always) nil)
   ;; Auto mode: only resolve if packages don't exist
   ((eq xcode-build-config-skip-package-resolution 'auto)
    (not (xcode-build-config-swift-packages-exist-p)))
   ;; Default to resolving
   (t t)))

(defun xcode-build-config-resolve-package-dependencies ()
  "Resolve Swift package dependencies if needed.
Returns the command string to include in the build command or empty string."
  (if (xcode-build-config-should-resolve-packages-p)
      (progn
        (message "Resolving Swift package dependencies...")
        "-resolvePackageDependencies \\")
    (progn
      (message "Skipping package resolution (packages exist or skip enabled)")
      "")))

(defun xcode-build-config-get-package-optimization-flags ()
  "Get package management optimization flags based on current settings.
Returns flags to optimize or skip package resolution."
  (if (not (xcode-build-config-should-resolve-packages-p))
      ;; When skipping resolution, use these flags to prevent automatic updates
      (concat
       "-skipPackageUpdates \\"  ; Skip updating packages
       ;; Note: -skipPackagePluginValidation is available in Xcode 14+
       ;; We'll try to use it and let xcodebuild ignore it if not supported
       "-skipPackagePluginValidation \\"  ; Skip plugin validation (Xcode 14+)
       "")
    ;; When resolving, don't add these flags
    ""))

(defun xcode-build-config-check-swift-packages-in-build ()
  "Check if Swift packages are being used/compiled in the build.
Checks both local .build and Xcode's DerivedData locations."
  (let* ((project-root (xcode-project-project-root))
         (local-source-packages (expand-file-name ".build/SourcePackages" project-root))
         (local-checkouts (expand-file-name ".build/checkouts" project-root))
         (build-intermediates (expand-file-name ".build/Build/Intermediates.noindex" project-root))
         ;; Find DerivedData for this project
         (derived-data-glob (format "%s/*%s*/SourcePackages"
                                   (expand-file-name xcode-build-config-xcode-cache-dir)
                                   (file-name-nondirectory (directory-file-name project-root))))
         (xcode-source-packages (car (file-expand-wildcards derived-data-glob))))
    (or
     ;; Check local .build/SourcePackages/repositories (where packages actually are)
     (let ((local-repos (expand-file-name "repositories" local-source-packages)))
       (and (file-exists-p local-repos)
            (> (length (directory-files local-repos nil "^[^.]" t)) 0)))
     ;; Check local .build/checkouts (SPM command line)
     (and (file-exists-p local-checkouts)
          (> (length (directory-files local-checkouts nil "^[^.]" t)) 0))
     ;; Check Xcode's DerivedData SourcePackages/repositories
     (let ((xcode-repos (when xcode-source-packages
                          (expand-file-name "repositories" xcode-source-packages))))
       (and xcode-repos
            (file-exists-p xcode-repos)
            (> (length (directory-files xcode-repos nil "^[^.]" t)) 0)))
     ;; Check build intermediates for package compilation
     (and (file-exists-p build-intermediates)
          (file-directory-p build-intermediates)
          (condition-case nil
              (> (length (directory-files build-intermediates nil "\\.build$" t)) 0)
            (error nil))))))

(defun xcode-build-config-should-resolve-packages-for-build ()
  "Determine if package resolution should be performed for .build-based builds.
For .build-based builds, we should resolve if packages aren't available locally in .build."
  (cond
   ;; Force resolution if explicitly requested
   (xcode-build-config-force-package-resolution t)
   ;; Check skip setting
   ((eq xcode-build-config-skip-package-resolution 'never) t)
   ((eq xcode-build-config-skip-package-resolution 'always) nil)
   ;; Auto mode: resolve if packages don't exist locally in .build
   ((eq xcode-build-config-skip-package-resolution 'auto)
    (let* ((project-root (xcode-project-project-root))
           (local-source-packages (expand-file-name ".build/SourcePackages" project-root))
           (local-repos (expand-file-name "repositories" local-source-packages))
           (local-checkouts (expand-file-name ".build/checkouts" project-root)))
      ;; Only skip resolution if packages exist LOCALLY in .build
      (not (or (and (file-exists-p local-repos)
                    (> (length (directory-files local-repos nil "^[^.]" t)) 0))
               (and (file-exists-p local-checkouts)
                    (> (length (directory-files local-checkouts nil "^[^.]" t)) 0))))))
   ;; Default to resolving
   (t t)))

(defun xcode-build-config-get-package-resolution-flags ()
  "Get package resolution flags for .build-based builds."
  (if (xcode-build-config-should-resolve-packages-for-build)
      (progn
        (message "Resolving Swift package dependencies for .build...")
        "-resolvePackageDependencies \\")
    (progn
      (message "Skipping package resolution (packages exist in .build)")
      "")))

(defun xcode-build-config-get-package-cache-flags ()
  "Get package caching flags to ensure packages are downloaded to .build"
  (concat
   (format "-packageCachePath %s \\" (expand-file-name xcode-build-config-package-cache-path)) ; Global package cache
   (format "-clonedSourcePackagesDirPath %s \\" (expand-file-name xcode-build-config-cloned-sources-path)) ; Cache cloned sources
   (if (not (xcode-build-config-should-resolve-packages-for-build))
       ;; When skipping resolution, add flags to prevent updates
       (concat
        "-skipPackageUpdates \\"
        "-skipPackagePluginValidation \\")
     "")))

;;; ============================================================================
;;; Environment Setup
;;; ============================================================================

(cl-defun xcode-build-config-setup-build-environment (&key for-device)
  "Setup optimal environment variables for build with aggressive caching and parallelization.
If FOR-DEVICE is non-nil, setup for device build (with signing), otherwise for simulator."
  ;; Enable all available caching mechanisms
  (setenv "SWIFT_BUILD_CACHE_ENABLED" "1")
  (setenv "CLANG_MODULE_CACHE_PATH" (expand-file-name "ModuleCache" xcode-build-config-swift-cache-dir))
  (setenv "SWIFT_MODULE_CACHE_PATH" (expand-file-name "ModuleCache" xcode-build-config-swift-cache-dir))
  (setenv "OBJC_DISABLE_INITIALIZE_FORK_SAFETY" "YES")  ;; Speeds up forking
  (setenv "CLANG_ENABLE_MODULES" "YES") ;; Precompiled headers
  (setenv "SWIFT_USE_PRECOMPILED_HEADERS" "YES")
  (setenv "GCC_PRECOMPILE_PREFIX_HEADER" "YES")
  (setenv "XCODE_BUILD_ENABLE_DEPENDENCIES" "YES") ;; Better dependency tracking

  ;; Disable index-while-building for faster builds
  (setenv "COMPILER_INDEX_STORE_ENABLE" "NO")
  (setenv "INDEX_ENABLE_BUILD_ARENA" "NO")
  (setenv "INDEX_ENABLE_DATA_STORE" "NO")

  ;; CocoaPods-specific optimizations
  (when (xcode-build-config-uses-cocoapods-p)
    (setenv "COCOAPODS_DISABLE_STATS" "YES")  ; Disable CocoaPods analytics
    (setenv "COCOAPODS_SKIP_CACHE" "NO")      ; Use CocoaPods cache
    (setenv "COCOAPODS_PARALLEL_CODE_SIGN" "YES"))  ; Parallel code signing

  ;; Don't set code signing environment variables here - handle it in the build command
  ;; This avoids conflicts between environment variables and command-line flags

  ;; Architecture settings - force arm64 only
  (setenv "ONLY_ACTIVE_ARCH" "YES")
  (setenv "ARCHS" "arm64")
  (setenv "VALID_ARCHS" "arm64")
  (setenv "EXCLUDED_ARCHS" "i386 x86_64")
  (setenv "SUPPORTED_PLATFORMS" "iphonesimulator iphoneos")

  ;; Explicitly disable all sanitizers
  (setenv "ENABLE_ADDRESS_SANITIZER" "NO")
  (setenv "ENABLE_THREAD_SANITIZER" "NO")
  (setenv "ENABLE_UNDEFINED_BEHAVIOR_SANITIZER" "NO")

  ;; Parallel compilation settings - more aggressive
  (let ((cores (max 1 (num-processors))))
    (setenv "SWIFT_MAX_PARALLEL_LTO_JOBS" (number-to-string cores))
    (setenv "SWIFT_PARALLEL_MODULE_JOBS" (number-to-string cores))
    (setenv "SWIFT_PARALLEL_COMPILE_JOBS" (number-to-string cores))
    (setenv "LLVM_PARALLEL_LINK_JOBS" (number-to-string (/ cores xcode-build-config-link-jobs-divisor)))
    (setenv "SWIFT_DRIVER_JOBS" (number-to-string (* cores xcode-build-config-parallel-jobs-multiplier)))
    ;; New: Use all cores for various tasks
    (setenv "GCC_PARALLEL_COMPILE_JOBS" (number-to-string cores))
    (setenv "CLANG_PARALLEL_COMPILE_JOBS" (number-to-string cores))
    (setenv "SWIFT_PARALLEL_SOURCEJOB_TASKS" (number-to-string cores)))

  ;; File system caching
  (setenv "XCODE_BUILD_SYSTEM_ENABLE_INCREMENTAL_DISTRIBUTED_CACHE" "YES")
  (setenv "XCODE_BUILD_SYSTEM_USE_WATCHERS" "YES")
  (setenv "XCODE_BUILD_SYSTEM_CACHE_DIR" (expand-file-name "ModuleCache" xcode-build-config-xcode-cache-dir))

  ;; Shared cache for all projects
  (setenv "SHARED_PRECOMPS_DIR" (expand-file-name "SharedPrecompiledHeaders" xcode-build-config-xcode-cache-dir))
  (setenv "OBJROOT" (expand-file-name "Build/Intermediates" xcode-build-config-xcode-cache-dir))

  ;; Incremental compilation optimizations
  (setenv "SWIFT_ENABLE_INCREMENTAL_COMPILATION" "YES")
  (setenv "SWIFT_OPTIMIZATION_LEVEL" "-O")
  (setenv "SWIFT_COMPILATION_MODE" "incremental")
  (setenv "SWIFT_WHOLE_MODULE_OPTIMIZATION" "NO")  ;; Better for incremental

  ;; Disable bitcode for faster builds
  (setenv "ENABLE_BITCODE" "NO")
  (setenv "BITCODE_GENERATION_MODE" "none")

  ;; Disable expensive checks
  (setenv "SWIFT_DISABLE_SAFETY_CHECKS" "1")
  (setenv "SWIFT_DISABLE_MODULE_CACHE_VALIDATION" "1")
  (setenv "SWIFT_SUPPRESS_WARNINGS" "YES")

  ;; Memory management
  (setenv "SWIFT_MEMORY_ALLOCATOR" "malloc")
  (setenv "SWIFT_DETERMINISTIC_HASHING" "1")

  ;; Increase memory limits for compiler processes
  (setenv "SWIFT_EXEC_MEMLIMIT" (number-to-string xcode-build-config-swift-exec-memlimit))
  (setenv "SWIFT_OPTIMIZATION_LEVEL_Release" "-Osize")

  ;; Debugging optimizations
  (setenv "SWIFT_SERIALIZE_DEBUGGING_OPTIONS" "NO")
  (setenv "SWIFT_REFLECTION_METADATA_LEVEL" "none")

  ;; Enable experimental features
  (setenv "SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY" "1")
  (setenv "SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED" "1")

  ;; Disable unnecessary validations
  (setenv "VALIDATE_PRODUCT" "NO")
  (setenv "VALIDATE_WORKSPACE" "NO"))

(defun xcode-build-config-enable-build-caching ()
  "Enable Xcode build system caching for faster incremental builds."
  (setenv "ENABLE_PRECOMPILED_HEADERS" "YES")
  (setenv "SWIFT_USE_DEVELOPMENT_SNAPSHOT" "NO")
  (setenv "SWIFT_USE_PRECOMPILED_HEADERS" "YES")
  (setenv "GCC_PRECOMPILE_PREFIX_HEADER" "YES")
  (setenv "GCC_USE_HEADER_SYMAP" "YES")
  (setenv "CLANG_USE_OPTIMIZATION_PROFILE" "YES")
  (setenv "SWIFT_USE_INCREMENTAL_COMPILATION" "YES")
  (setenv "SWIFT_COMPILER_INDEX_STORE_ENABLE" "NO") ;; Disable indexing during build
  (setenv "XCODE_BUILD_SYSTEM_FORCE_ENABLE_PCH_VALIDATION" "NO")
  (setenv "SWIFT_DRIVER_JOBS" (number-to-string (* (num-processors) xcode-build-config-parallel-jobs-multiplier)))
  (setenv "XCODE_BUILD_SYSTEM_SHOW_ENV" "NO")

  ;; Generate temporary xcconfig file with optimized settings
  (xcode-build-config-generate-fast-build-xcconfig))

(defun xcode-build-config-generate-fast-build-xcconfig ()
  "Generate a temporary xcconfig file with optimized build settings."
  (with-temp-file "/tmp/fast-build.xcconfig"
    (insert "// Temporary xcconfig for faster builds\n\n")

    ;; Optimization settings
    (insert "// Optimization settings\n")
    (insert (format "OTHER_SWIFT_FLAGS = %s\n"
                    (mapconcat (lambda (flag) (concat "-Xfrontend " flag))
                               xcode-build-config-other-swift-flags " ")))
    (insert "SWIFT_OPTIMIZATION_LEVEL = -Osize\n")
    (insert "GCC_OPTIMIZATION_LEVEL = 0\n")
    (insert "SWIFT_COMPILATION_MODE = incremental\n")
    (insert "SWIFT_WHOLE_MODULE_OPTIMIZATION = NO\n\n")

    ;; Caching settings
    (insert "// Caching settings\n")
    (insert "SWIFT_DEPENDENCY_CACHE_ENABLED = YES\n")
    (insert "SWIFT_CACHE_COMPILE_JOB = YES\n")
    (insert "ENABLE_PRECOMPILED_HEADERS = YES\n\n")

    ;; Parallelization
    (insert "// Parallelization\n")
    (insert (format "SWIFT_PARALLEL_MODULE_JOBS = %d\n" (num-processors)))
    (insert (format "SWIFT_PARALLEL_COMPILE_JOBS = %d\n" (num-processors)))
    (insert "SWIFT_USE_PARALLEL_WHOLE_MODULE_OPTIMIZATION = YES\n")
    (insert "SWIFT_USE_PARALLEL_SOURCEJOB_TASKS = YES\n\n")

    ;; Architecture settings
    (insert "// Architecture settings\n")
    (insert "ONLY_ACTIVE_ARCH = YES\n")
    (insert "ARCHS = arm64\n")
    (insert "VALID_ARCHS = arm64\n")
    (insert "EXCLUDED_ARCHS = i386 x86_64\n\n")

    ;; Disable unnecessary features
    (insert "// Disable unnecessary features\n")
    (insert "COMPILER_INDEX_STORE_ENABLE = NO\n")
    (insert "ENABLE_BITCODE = NO\n")
    (insert "CODE_SIGNING_REQUIRED = NO\n")
    (insert "CODE_SIGN_IDENTITY = \"\"\n")
    (insert "ENABLE_TESTABILITY = NO\n")
    (insert "ENABLE_PREVIEWS = NO\n")
    (insert "ENABLE_ADDRESS_SANITIZER = NO\n")
    (insert "ENABLE_THREAD_SANITIZER = NO\n")
    (insert "ENABLE_UNDEFINED_BEHAVIOR_SANITIZER = NO\n")
    (insert "SWIFT_TREAT_WARNINGS_AS_ERRORS = NO\n")
    (insert "SWIFT_SUPPRESS_WARNINGS = YES\n")
    (insert "SWIFT_ENFORCE_EXCLUSIVE_ACCESS = off\n")
    (insert "SWIFT_STRICT_CONCURRENCY = minimal\n")
    (insert "SWIFT_REFLECTION_METADATA_LEVEL = none\n")
    (insert "SWIFT_SERIALIZE_DEBUGGING_OPTIONS = NO\n")
    (insert "INDEX_ENABLE_DATA_STORE = NO\n")
    (insert "RUN_CLANG_STATIC_ANALYZER = NO\n")
    (insert "CLANG_ENABLE_CODE_COVERAGE = NO\n")
    (insert "BUILD_LIBRARY_FOR_DISTRIBUTION = NO\n\n")

    ;; LTO settings
    (insert "// LTO settings\n")
    (insert (if xcode-build-config-use-thin-lto
                "LLVM_LTO = Thin\n"
              "LLVM_LTO = NO\n"))
    (insert "\n")

    ;; Debug info
    (insert "// Debug info\n")
    (insert "DEBUG_INFORMATION_FORMAT = dwarf\n")
    (insert "GCC_GENERATE_DEBUGGING_SYMBOLS = YES\n")
    (insert "STRIP_INSTALLED_PRODUCT = NO\n")
    (insert "DEPLOYMENT_POSTPROCESSING = NO\n")
    (insert "COPY_PHASE_STRIP = NO\n")))

;;; ============================================================================
;;; Build Command Construction
;;; ============================================================================

(defun xcode-build-config-xcodebuild-command ()
  "Get xcodebuild command, possibly with x86 architecture."
  (if xcode-build-config--current-environment-x86
      "env /usr/bin/arch -x86_64 xcrun xcodebuild build"
    "xcrun xcodebuild build"))

(cl-defun xcode-build-config-build-app-command (&key sim-id device-id derived-path)
  "Generate optimized xcodebuild command with aggressive parallelization and CocoaPods support.
SIM-ID is the simulator identifier, DEVICE-ID is the physical device identifier,
DERIVED-PATH is the derived data path."
  (if xcode-build-config--current-build-command
      xcode-build-config--current-build-command
    (let ((workspace-or-project (xcode-project-get-workspace-or-project)))
      (mapconcat 'identity
                 (delq nil
                       (list
                        "xcrun xcodebuild build"
                        (format "%s" workspace-or-project)
                        (format "-scheme '%s'" (xcode-project-scheme-display-name))
                        ;; NOTE: -resolvePackageDependencies cannot be used with build command!
                        ;; It makes xcodebuild ONLY resolve packages and then exit
                        ;; Package cache paths - use Xcode defaults for best compatibility
                        ;; Xcode will handle package placement automatically
                        ;; Skip updates if packages exist (only for subsequent builds)
                        (when (not (xcode-build-config-should-resolve-packages-for-build))
                          "-skipPackageUpdates")
                        ;; Performance optimizations (but allow package downloads when needed)
                        "-skipMacroValidation"  ; Skip macro validation (Xcode 15+)
                        ;; Parallelization
                        "-parallelizeTargets"
                        (format "-jobs %d" (* (num-processors) xcode-build-config-parallel-jobs-multiplier))
                        ;; Destination
                        (cond
                         (sim-id
                          (format "-destination 'platform=iOS Simulator,id=%s'" sim-id))
                         (device-id
                          (format "-destination 'platform=iOS,id=%s'" device-id)))
                        ;; Configuration if specified
                        (when xcode-build-config-default-configuration
                          (format "-configuration %s" xcode-build-config-default-configuration))
                        ;; Code signing for simulator
                        (when sim-id
                          "CODE_SIGNING_REQUIRED=NO")
                        (when sim-id
                          "CODE_SIGN_IDENTITY=\"\"")
                        (when sim-id
                          "CODE_SIGNING_ALLOWED=NO")
                        (when sim-id
                          "DEVELOPMENT_TEAM=\"\"")
                        ;; Other build flags
                        "-skipUnavailableActions"
                        "-useNewBuildSystem=YES"
                        ;; Optimization flags as a list (only if they exist)
                        (when (and xcode-build-config-other-swift-flags
                                  (> (length xcode-build-config-other-swift-flags) 0))
                          (format "OTHER_SWIFT_FLAGS=\"%s\""
                                  (mapconcat (lambda (flag) (concat "-Xfrontend " flag))
                                            xcode-build-config-other-swift-flags " ")))
                        ;; Linker flags (for hot reload support)
                        (when (and xcode-build-config-other-ld-flags
                                  (> (length xcode-build-config-other-ld-flags) 0))
                          (format "OTHER_LDFLAGS=\"%s\""
                                  (mapconcat 'identity xcode-build-config-other-ld-flags " ")))
                        "SWIFT_BUILD_CACHE_ENABLED=YES"
                        (format "SWIFT_PARALLEL_MODULE_JOBS=%d" (num-processors))
                        "SWIFT_USE_PARALLEL_SOURCEJOB_TASKS=YES"
                        "SWIFT_COMPILATION_MODE=incremental"
                        "SWIFT_SERIALIZE_DEBUGGING_OPTIONS=NO"
                        "SWIFT_ENABLE_BUILD_CACHE=YES"
                        "ENABLE_PREVIEWS=NO"
                        "INDEX_ENABLE_DATA_STORE=NO"
                        "SWIFTPM_ENABLE_BUILD_CACHES=1"
                        "COMPILER_INDEX_STORE_ENABLE=NO"
                        "ONLY_ACTIVE_ARCH=YES"
                        "ENABLE_TESTABILITY=NO"
                        "ENABLE_BITCODE=NO"
                        "DEBUG_INFORMATION_FORMAT=dwarf"
                        "RUN_CLANG_STATIC_ANALYZER=NO"
                        "CLANG_ENABLE_CODE_COVERAGE=NO"
                        "GCC_GENERATE_DEBUGGING_SYMBOLS=YES"
                        "BUILD_LIBRARY_FOR_DISTRIBUTION=NO"
                        (if xcode-build-config-use-thin-lto
                            "LLVM_LTO=Thin"
                          "LLVM_LTO=NO")
                        "STRIP_INSTALLED_PRODUCT=NO"
                        "DEPLOYMENT_POSTPROCESSING=NO"
                        "COPY_PHASE_STRIP=NO"
                        ;; Derived data path
                        "-derivedDataPath .build"))
                 " "))))

;;; ============================================================================
;;; Optimization Flags
;;; ============================================================================

(defun xcode-build-config-get-optimization-flags ()
  "Get optimization flags based on current optimization level."
  `(
    ,(format "OTHER_SWIFT_FLAGS=\"%s\""
             (mapconcat (lambda (flag) (concat "-Xfrontend " flag))
                        xcode-build-config-other-swift-flags " "))
    "SWIFT_BUILD_CACHE_ENABLED=YES"  ; Explicitly enable Swift build cache
    ,(format "SWIFT_PARALLEL_MODULE_JOBS=%d" (num-processors))  ; Use normal CPU cores, not double
    "SWIFT_USE_PARALLEL_SOURCEJOB_TASKS=YES"
    "SWIFT_COMPILATION_MODE=incremental"
    "SWIFT_SERIALIZE_DEBUGGING_OPTIONS=NO"
    "SWIFT_ENABLE_BUILD_CACHE=YES"
    "ENABLE_PREVIEWS=NO"
    "INDEX_ENABLE_DATA_STORE=NO"
    "SWIFTPM_ENABLE_BUILD_CACHES=1"
    "COMPILER_INDEX_STORE_ENABLE=NO"
    "ONLY_ACTIVE_ARCH=YES"
    "ENABLE_TESTABILITY=NO"
    "ENABLE_BITCODE=NO"
    "DEBUG_INFORMATION_FORMAT=dwarf"
    ;; Conservative optimizations that shouldn't break packages
    "RUN_CLANG_STATIC_ANALYZER=NO"  ; Disable Clang static analyzer
    "CLANG_ENABLE_CODE_COVERAGE=NO"  ; Disable code coverage
    "GCC_GENERATE_DEBUGGING_SYMBOLS=YES"  ; Keep symbols but optimize
    "BUILD_LIBRARY_FOR_DISTRIBUTION=NO"  ; Skip library evolution support
    ;; Conditional Thin LTO for faster linking
    ,(if xcode-build-config-use-thin-lto
         "LLVM_LTO=Thin"  ; Enable Thin LTO when requested
       "LLVM_LTO=NO")  ; Disable LTO for faster Debug builds
    "STRIP_INSTALLED_PRODUCT=NO"  ; Skip stripping in Debug
    "DEPLOYMENT_POSTPROCESSING=NO"  ; Skip deployment processing
    "COPY_PHASE_STRIP=NO"))

;;; ============================================================================
;;; Reset Functions
;;; ============================================================================

(defun xcode-build-config-reset ()
  "Reset all build configuration cache."
  (setq xcode-build-config--current-build-command nil
        xcode-build-config--current-environment-x86 nil))

(provide 'xcode-build-config)
;;; xcode-build-config.el ends here
