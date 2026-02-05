#!/usr/bin/env ruby
# frozen_string_literal: true

# preview-inject-target.rb - Inject PreviewHost target into Xcode project
#
# This script creates a temporary PreviewHost app target in an existing
# Xcode project to build and preview SwiftUI views.
#
# Usage:
#   ruby preview-inject-target.rb --project /path/to/Project.xcodeproj \
#                                  --preview-dir /tmp/PreviewHost \
#                                  --module ModuleName \
#                                  --imports "SwiftUI,ModuleName"
#
# Or via JSON stdin:
#   echo '{"project_path": "...", ...}' | ruby preview-inject-target.rb --json
#
# Based on Claude-XcodePreviews (MIT License)
# https://github.com/Iron-Ham/Claude-XcodePreviews

require 'json'
require 'fileutils'
require 'set'
require 'pathname'

# Force using user-installed gems first (Homebrew Ruby)
gem_paths = [
  File.expand_path('~/.gem/ruby/4.0.0'),
  File.expand_path('~/.gem/ruby/3.3.0'),
  File.expand_path('~/.gem/ruby/3.2.0'),
  File.expand_path('~/.gem/ruby/3.1.0'),
  File.expand_path('~/.gem/ruby/3.0.0')
]

# Prepend user gem paths to load path BEFORE requiring xcodeproj
gem_paths.each do |path|
  gems_dir = File.join(path, 'gems')
  next unless File.directory?(gems_dir)
  
  Dir.glob(File.join(gems_dir, '*', 'lib')).sort.reverse.each do |lib|
    $LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
  end
end

begin
  require 'xcodeproj'
rescue LoadError => e
  puts JSON.generate({ success: false, error: "xcodeproj gem not found: #{e.message}. Install with: gem install xcodeproj --user-install" })
  exit 1
end

# Check xcodeproj version - need 1.26+ for Xcode 16 support
xcodeproj_version = Gem::Version.new(Xcodeproj::VERSION)
if xcodeproj_version < Gem::Version.new('1.26.0')
  puts JSON.generate({ 
    success: false, 
    error: "xcodeproj gem version #{Xcodeproj::VERSION} is too old. Need 1.26.0+ for Xcode 16 support. Run: gem install xcodeproj --user-install" 
  })
  exit 1
end

class PreviewTargetInjector
  PREVIEW_TARGET_NAME = 'PreviewHost'
  PREVIEW_BUNDLE_ID = 'com.swift-development.preview-host'
  
  def initialize(options)
    @project_path = options[:project_path]
    @preview_dir = options[:preview_dir]
    @module_name = options[:module_name]
    @imports = options[:imports] || []
    @deployment_target = options[:deployment_target] || '17.0'
    @verbose = options[:verbose] || false
    @source_file = options[:source_file]  # Original Swift file being previewed
  end
  
  def run
    log "Opening project: #{@project_path}"
    @project = Xcodeproj::Project.open(@project_path)
    
    # Remove existing PreviewHost if present
    cleanup_existing_target
    
    # Create new target
    create_preview_target
    
    # Configure build settings
    configure_build_settings
    
    # Add source files
    add_source_files
    
    # Add dependencies
    add_dependencies
    
    # NOTE: We don't add resource bundles here because SPM handles them automatically
    # when we add package_product_dependencies. The bundles are built as part of the
    # package build process, not copied separately.
    
    # Save project
    @project.save
    log "Project saved"
    
    # Create scheme
    create_scheme
    
    result = { 
      success: true, 
      scheme: PREVIEW_TARGET_NAME,
      deployment_target: @deployment_target,
      bundle_id: PREVIEW_BUNDLE_ID
    }
    result[:source_target] = @source_target_name if @source_target_name
    result
  rescue StandardError => e
    { success: false, error: e.message, backtrace: e.backtrace.first(5) }
  end
  
  private
  
  def log(msg)
    STDERR.puts "[preview-inject] #{msg}" if @verbose
  end
  
  def cleanup_existing_target
    existing = @project.targets.find { |t| t.name == PREVIEW_TARGET_NAME }
    if existing
      log "Removing existing #{PREVIEW_TARGET_NAME} target"
      existing.remove_from_project
    end
    
    # Remove existing group
    @project.main_group.groups.each do |g|
      if g.name == PREVIEW_TARGET_NAME
        g.remove_from_project
        break
      end
    end
    
    # Remove existing scheme
    scheme_path = File.join(@project_path, 'xcshareddata', 'xcschemes', "#{PREVIEW_TARGET_NAME}.xcscheme")
    FileUtils.rm_f(scheme_path)
  end
  
  def create_preview_target
    log "Creating #{PREVIEW_TARGET_NAME} target"
    
    # Detect deployment target from existing targets
    detect_deployment_target
    
    @target = @project.new_target(:application, PREVIEW_TARGET_NAME, :ios, @deployment_target)
    log "Target created with iOS #{@deployment_target}"
  end
  
  def detect_deployment_target
    @project.targets.each do |target|
      next if target.name == PREVIEW_TARGET_NAME
      target.build_configurations.each do |config|
        dt = config.build_settings['IPHONEOS_DEPLOYMENT_TARGET']
        if dt && !dt.empty?
          @deployment_target = dt
          log "Detected deployment target: iOS #{@deployment_target}"
          return
        end
      end
    end
  end
  
  def configure_build_settings
    @target.build_configurations.each do |config|
      config.build_settings['PRODUCT_NAME'] = PREVIEW_TARGET_NAME
      config.build_settings['PRODUCT_MODULE_NAME'] = PREVIEW_TARGET_NAME
      config.build_settings['PRODUCT_BUNDLE_IDENTIFIER'] = PREVIEW_BUNDLE_ID
      config.build_settings['GENERATE_INFOPLIST_FILE'] = 'YES'
      config.build_settings['INFOPLIST_KEY_UIApplicationSceneManifest_Generation'] = 'YES'
      config.build_settings['INFOPLIST_KEY_UILaunchScreen_Generation'] = 'YES'
      config.build_settings['SWIFT_VERSION'] = '5.0'
      config.build_settings['CODE_SIGN_STYLE'] = 'Automatic'
      config.build_settings['IPHONEOS_DEPLOYMENT_TARGET'] = @deployment_target
      # Allow finding modules from dependencies - include DerivedData paths
      config.build_settings['SWIFT_INCLUDE_PATHS'] = '$(inherited) $(BUILT_PRODUCTS_DIR)'
      # Framework search paths for SPM-built frameworks
      config.build_settings['FRAMEWORK_SEARCH_PATHS'] = '$(inherited) $(BUILT_PRODUCTS_DIR)'
      # Enable testability for @testable imports
      config.build_settings['ENABLE_TESTABILITY'] = 'YES'
      # Ensure we build for active architecture only (faster)
      config.build_settings['ONLY_ACTIVE_ARCH'] = 'YES'
    end
    log "Build settings configured"
  end
  
  def add_source_files
    return unless @preview_dir && File.directory?(@preview_dir)
    
    # Create group for preview files
    preview_group = @project.main_group.new_group(PREVIEW_TARGET_NAME, @preview_dir)
    
    # Add all Swift files in preview directory
    Dir.glob(File.join(@preview_dir, '*.swift')).each do |swift_file|
      ref = preview_group.new_file(swift_file)
      @target.source_build_phase.add_file_reference(ref)
      log "Added source: #{File.basename(swift_file)}"
    end
  end
  
  def find_target_for_source_file
    # Find which target contains the source file
    return nil unless @source_file && File.exist?(@source_file)
    
    absolute_path = File.expand_path(@source_file)
    
    @project.targets.each do |target|
      next if target.name == PREVIEW_TARGET_NAME
      
      target.source_build_phase&.files&.each do |build_file|
        file_ref = build_file.file_ref
        next unless file_ref&.path
        
        ref_path = file_ref.real_path.to_s rescue nil
        if ref_path == absolute_path
          log "Source file belongs to target: #{target.name}"
          return target
        end
      end
    end
    
    # Source file not found in any target - return nil
    # (caller will handle SPM package case)
    nil
  end
  
  def find_main_app_target
    # Find the main application target in the project
    # Prefer target matching project name, otherwise first app target
    project_name = File.basename(@project_path, '.xcodeproj')
    
    # First try exact match with project name
    app_target = @project.targets.find do |t|
      t.name == project_name && t.product_type == 'com.apple.product-type.application'
    end
    
    # Otherwise find any app target (excluding PreviewHost)
    app_target ||= @project.targets.find do |t|
      t.name != PREVIEW_TARGET_NAME && t.product_type == 'com.apple.product-type.application'
    end
    
    app_target
  end
  
  def add_dependencies
    dep_targets = []
    
    # Find targets matching imports
    @imports.each do |imp|
      target = @project.targets.find { |t| t.name == imp }
      # Only add non-app targets as dependencies (apps can't be easily linked)
      if target && !target.product_type&.include?('application')
        dep_targets << target
      end
    end
    
    # Also add module target if specified and not already included
    if @module_name && !dep_targets.any? { |t| t.name == @module_name }
      module_target = @project.targets.find { |t| t.name == @module_name }
      if module_target && !module_target.product_type&.include?('application')
        dep_targets << module_target
      end
    end
    
    # Find the target containing the source file (if provided)
    if @source_file
      source_target = find_target_for_source_file
      if source_target
        if source_target.product_type&.include?('application')
          # For app targets, copy source files instead of adding dependency
          log "Source file is in app target: #{source_target.name}"
          copy_source_files_from_target(source_target)
        elsif !dep_targets.any? { |t| t.name == source_target.name }
          dep_targets << source_target
          log "Found source file target: #{source_target.name}"
          @source_target_name = source_target.name
        end
      else
        # Source file not found in any target - likely in an SPM package
        # Find the main app target to get SPM dependencies
        log "Source file not found in project targets, checking for SPM package"
        app_target = find_main_app_target
        if app_target
          log "Using main app target for SPM dependencies: #{app_target.name}"
          copy_spm_dependencies_from_target(app_target)
          copy_assets_from_target(app_target)
          @source_target_name = "SPM:#{@module_name}"
        end
      end
    end
    
    log "Found #{dep_targets.length} dependency targets"
    
    dep_targets.each do |dep|
      # Add target dependency (ensures build order)
      @target.add_dependency(dep)
      log "Added dependency: #{dep.name}"
      
      # For static libraries, also link
      if dep.product_type == 'com.apple.product-type.library.static'
        @target.frameworks_build_phase.add_file_reference(dep.product_reference) if dep.product_reference
        log "Linked static library: #{dep.name}"
      end
    end
    
    @dep_targets = dep_targets
  end
  
  def copy_source_files_from_target(app_target)
    # For app targets, we copy the source file to PreviewHost
    # This allows previewing views from app targets without dependencies
    return unless @source_file && File.exist?(@source_file)
    
    # Find the preview group we created
    preview_group = @project.main_group.groups.find { |g| g.name == PREVIEW_TARGET_NAME }
    return unless preview_group
    
    # Read and process the source file
    source_content = File.read(@source_file)
    
    # Remove @main struct using a more robust approach
    processed_content = source_content.gsub(/@main\s*\n/, "// @main removed for preview\n")
    
    # Remove #Preview blocks with proper brace matching
    processed_content = remove_preview_blocks(processed_content)
    
    # Write processed file to preview directory
    processed_filename = "Source_#{File.basename(@source_file)}"
    processed_path = File.join(@preview_dir, processed_filename)
    File.write(processed_path, processed_content)
    
    # Add to target
    ref = preview_group.new_file(processed_path)
    @target.source_build_phase.add_file_reference(ref)
    log "Copied and processed source file: #{processed_filename}"
    
    # Copy assets from app target
    copy_assets_from_target(app_target)
    
    # Copy SPM package dependencies from app target
    copy_spm_dependencies_from_target(app_target)
    
    @source_target_name = app_target.name
  end
  
  def copy_spm_dependencies_from_target(source_target)
    # Copy SPM package product dependencies from source target to PreviewHost
    # This allows PreviewHost to import the same modules as the source target
    
    return unless source_target.respond_to?(:package_product_dependencies)
    
    source_deps = source_target.package_product_dependencies
    if source_deps.nil? || source_deps.empty?
      log "No SPM package dependencies found in #{source_target.name}"
      # Try to find dependencies from other targets
      find_and_copy_all_spm_dependencies
      return
    end
    
    log "Found #{source_deps.length} SPM package dependencies in #{source_target.name}"
    log "SPM dependencies: #{source_deps.map(&:product_name).join(', ')}"
    
    # IMPORTANT: Copy ALL SPM dependencies, not just the ones in imports
    # This ensures transitive dependencies (e.g., KYC -> BruceLocalization) are available
    # The issue is that if we only copy "KYC", the modules it depends on won't be found
    deps_to_copy = source_deps
    
    log "Copying all #{deps_to_copy.length} SPM dependencies to PreviewHost (including transitive)"
    
    # Get frameworks build phase
    frameworks_phase = @target.frameworks_build_phase
    
    # Track which dependencies we've added to avoid duplicates
    added_deps = Set.new
    
    deps_to_copy.each do |dep|
      next if added_deps.include?(dep.product_name)
      
      begin
        # Create a new package product dependency for PreviewHost
        new_dep = @project.new(Xcodeproj::Project::Object::XCSwiftPackageProductDependency)
        new_dep.product_name = dep.product_name
        new_dep.package = dep.package  # Reference to the same remote package (if any)
        
        @target.package_product_dependencies << new_dep
        
        # Also add to frameworks build phase for linking
        # Create a build file that references this product dependency
        build_file = @project.new(Xcodeproj::Project::Object::PBXBuildFile)
        build_file.product_ref = new_dep
        frameworks_phase.files << build_file
        
        added_deps << dep.product_name
        log "Added and linked SPM dependency: #{dep.product_name}"
      rescue StandardError => e
        log "Warning: Failed to copy SPM dependency #{dep.product_name}: #{e.message}"
      end
    end
    
    # Also check if there are any local SPM packages that need to be added
    # These might be in Packages/ directory or referenced via local path
    add_local_package_dependencies(source_target)
    
    # Check if all requested imports are covered
    check_missing_imports(added_deps)
  end
  
  def find_and_copy_all_spm_dependencies
    # Find all SPM dependencies from ALL targets in the project
    # This is a fallback when the source target doesn't have direct dependencies
    log "Searching all project targets for SPM dependencies..."
    
    frameworks_phase = @target.frameworks_build_phase
    added_deps = Set.new
    
    @project.targets.each do |target|
      next if target.name == PREVIEW_TARGET_NAME
      next unless target.respond_to?(:package_product_dependencies)
      
      deps = target.package_product_dependencies
      next if deps.nil? || deps.empty?
      
      log "Found #{deps.length} SPM deps in target #{target.name}: #{deps.map(&:product_name).join(', ')}"
      
      deps.each do |dep|
        next if added_deps.include?(dep.product_name)
        
        begin
          new_dep = @project.new(Xcodeproj::Project::Object::XCSwiftPackageProductDependency)
          new_dep.product_name = dep.product_name
          new_dep.package = dep.package
          
          @target.package_product_dependencies << new_dep
          
          build_file = @project.new(Xcodeproj::Project::Object::PBXBuildFile)
          build_file.product_ref = new_dep
          frameworks_phase.files << build_file
          
          added_deps << dep.product_name
          log "Added SPM dependency from #{target.name}: #{dep.product_name}"
        rescue StandardError => e
          log "Warning: Failed to copy SPM dependency #{dep.product_name}: #{e.message}"
        end
      end
    end
    
    check_missing_imports(added_deps)
  end
  
  def check_missing_imports(added_deps)
    # Check if all requested imports are covered by the added dependencies
    return unless @imports && !@imports.empty?
    
    # Standard frameworks that don't need SPM
    standard_frameworks = Set.new(%w[
      SwiftUI UIKit Foundation Combine CoreData CoreGraphics
      CoreImage CoreLocation MapKit AVFoundation Photos
      PhotosUI StoreKit WidgetKit AppIntents
    ])
    
    missing = @imports.reject do |imp|
      added_deps.include?(imp) || standard_frameworks.include?(imp)
    end
    
    if missing.any?
      log "WARNING: The following imports may not be available: #{missing.join(', ')}"
      log "These modules need to be SPM dependencies in the project"
    end
  end
  
  def add_local_package_dependencies(source_target)
    # Find local package references in the project
    # These are XCLocalSwiftPackageReference objects
    
    local_packages = nil
    if @project.root_object.respond_to?(:local_packages)
      local_packages = @project.root_object.local_packages
    end
    
    # Also check for package references (both local and remote)
    package_refs = nil
    if @project.root_object.respond_to?(:package_references)
      package_refs = @project.root_object.package_references
    end
    
    log "Local packages: #{local_packages&.length || 0}"
    log "Package references: #{package_refs&.length || 0}"
    
    # Get frameworks build phase
    frameworks_phase = @target.frameworks_build_phase
    
    # Get existing dependency names to avoid duplicates
    existing_deps = @target.package_product_dependencies.map(&:product_name).to_set
    
    # Process local packages from project
    if local_packages && !local_packages.empty?
      local_packages.each do |pkg_ref|
        next unless pkg_ref.respond_to?(:relative_path)
        
        pkg_path = pkg_ref.relative_path
        log "Found local package: #{pkg_path}"
        
        # Try to find products from this package that we need
        add_products_from_package(pkg_ref, existing_deps, frameworks_phase)
      end
    end
    
    # Process remote package references
    if package_refs && !package_refs.empty?
      package_refs.each do |pkg_ref|
        name = pkg_ref.respond_to?(:name) ? pkg_ref.name : nil
        url = pkg_ref.respond_to?(:repositoryURL) ? pkg_ref.repositoryURL : nil
        log "Found package reference: #{name || url || 'unknown'}"
      end
    end
    
    # IMPORTANT: Also search for local packages on disk that might not be
    # registered as XCLocalSwiftPackageReference but are still used
    find_local_packages_on_disk(existing_deps, frameworks_phase)
  end
  
  def find_local_packages_on_disk(existing_deps, frameworks_phase)
    # Search for Package.swift files in common locations relative to project
    project_dir = File.dirname(@project_path)
    
    # Common locations for local packages
    search_dirs = [
      project_dir,
      File.join(project_dir, '..'),  # Parent directory
    ]
    
    # Also check directories that match main group children
    @project.main_group.children.each do |child|
      next unless child.respond_to?(:path) && child.path
      child_path = File.join(project_dir, child.path)
      search_dirs << child_path if File.directory?(child_path)
    end
    
    found_packages = Set.new
    
    search_dirs.uniq.each do |dir|
      next unless File.directory?(dir)
      
      # Find Package.swift files (max depth 3 to avoid going too deep)
      Dir.glob(File.join(dir, '**/Package.swift')).each do |pkg_manifest|
        # Skip if too deep
        relative_depth = pkg_manifest.sub(dir, '').count('/')
        next if relative_depth > 3
        
        pkg_dir = File.dirname(pkg_manifest)
        pkg_name = File.basename(pkg_dir)
        
        next if found_packages.include?(pkg_name)
        found_packages << pkg_name
        
        # Check if this package provides any of our missing imports
        check_and_add_local_package(pkg_dir, pkg_name, existing_deps, frameworks_phase)
      end
    end
  end
  
  def check_and_add_local_package(pkg_dir, pkg_name, existing_deps, frameworks_phase)
    # Read Package.swift to find product names
    pkg_manifest = File.join(pkg_dir, 'Package.swift')
    return unless File.exist?(pkg_manifest)
    
    content = File.read(pkg_manifest)
    
    # Extract product names from Package.swift
    # Look for patterns like: .library(name: "BruceUI", ...)
    products = []
    content.scan(/\.library\s*\(\s*name:\s*"([^"]+)"/) do |match|
      products << match[0]
    end
    content.scan(/\.executable\s*\(\s*name:\s*"([^"]+)"/) do |match|
      products << match[0]
    end
    
    # Also consider the package directory name as a potential product
    products << pkg_name unless products.include?(pkg_name)
    
    log "Local package '#{pkg_name}' provides products: #{products.join(', ')}" if products.any?
    
    # Check if any of our imports match these products
    return unless @imports && !@imports.empty?
    
    matching_imports = @imports & products
    return if matching_imports.empty?
    
    log "Found matching imports in local package '#{pkg_name}': #{matching_imports.join(', ')}"
    
    # Create a local package reference if it doesn't exist
    local_pkg_ref = find_or_create_local_package_reference(pkg_dir)
    return unless local_pkg_ref
    
    # Add each matching product
    matching_imports.each do |product_name|
      next if existing_deps.include?(product_name)
      
      begin
        new_dep = @project.new(Xcodeproj::Project::Object::XCSwiftPackageProductDependency)
        new_dep.product_name = product_name
        new_dep.package = local_pkg_ref
        
        @target.package_product_dependencies << new_dep
        
        build_file = @project.new(Xcodeproj::Project::Object::PBXBuildFile)
        build_file.product_ref = new_dep
        frameworks_phase.files << build_file
        
        existing_deps << product_name
        log "Added local package product: #{product_name} from #{pkg_name}"
      rescue StandardError => e
        log "Warning: Could not add #{product_name} from local package: #{e.message}"
      end
    end
  end
  
  def find_or_create_local_package_reference(pkg_dir)
    project_dir = File.dirname(@project_path)
    relative_path = Pathname.new(pkg_dir).relative_path_from(Pathname.new(project_dir)).to_s
    
    # Check if we already have a reference to this package
    if @project.root_object.respond_to?(:local_packages)
      existing = @project.root_object.local_packages&.find do |pkg|
        pkg.respond_to?(:relative_path) && pkg.relative_path == relative_path
      end
      return existing if existing
    end
    
    # Create a new local package reference
    begin
      local_pkg_ref = @project.new(Xcodeproj::Project::Object::XCLocalSwiftPackageReference)
      local_pkg_ref.relative_path = relative_path
      
      # Add to project's local packages
      if @project.root_object.respond_to?(:local_packages)
        @project.root_object.local_packages ||= []
        @project.root_object.local_packages << local_pkg_ref
      end
      
      log "Created local package reference: #{relative_path}"
      local_pkg_ref
    rescue StandardError => e
      log "Warning: Could not create local package reference for #{pkg_dir}: #{e.message}"
      nil
    end
  end
  
  def add_products_from_package(pkg_ref, existing_deps, frameworks_phase)
    # Try to find which products from this package we need
    # Check if any of our imports match products from this package
    return unless @imports && !@imports.empty?
    
    # Get the package's products by checking if they match our imports
    @imports.each do |imp|
      next if existing_deps.include?(imp)
      
      # Check if this import could come from this package
      # For local packages, we try to add it and see if it works
      begin
        new_dep = @project.new(Xcodeproj::Project::Object::XCSwiftPackageProductDependency)
        new_dep.product_name = imp
        new_dep.package = pkg_ref
        
        @target.package_product_dependencies << new_dep
        
        build_file = @project.new(Xcodeproj::Project::Object::PBXBuildFile)
        build_file.product_ref = new_dep
        frameworks_phase.files << build_file
        
        existing_deps << imp
        log "Added local package product: #{imp}"
      rescue StandardError => e
        log "Could not add #{imp} from local package: #{e.message}"
      end
    end
  end
  
  def remove_preview_blocks(content)
    # Remove #Preview blocks with proper brace matching
    result = ""
    i = 0
    while i < content.length
      # Check for #Preview
      if content[i, 8] == "#Preview"
        # Find opening brace
        j = i + 8
        while j < content.length && content[j] != '{'
          j += 1
        end
        
        if j < content.length
          # Count braces to find matching close
          depth = 1
          k = j + 1
          while k < content.length && depth > 0
            if content[k] == '{'
              depth += 1
            elsif content[k] == '}'
              depth -= 1
            elsif content[k] == '"'
              # Skip string literals
              k += 1
              while k < content.length && content[k] != '"'
                k += 1 if content[k] == '\\'  # Skip escaped chars
                k += 1
              end
            end
            k += 1
          end
          
          # Replace with comment
          result += "// #Preview removed for preview\n"
          i = k
          next
        end
      end
      
      result += content[i]
      i += 1
    end
    result
  end
  
  def copy_assets_from_target(app_target)
    # For Xcode 16+ projects using PBXFileSystemSynchronizedRootGroup,
    # assets are not explicitly listed in the project file.
    # We need to find them on disk and copy them.
    
    preview_resources = @target.build_phases.find { |p| p.is_a?(Xcodeproj::Project::Object::PBXResourcesBuildPhase) }
    preview_group = @project.main_group.groups.find { |g| g.name == PREVIEW_TARGET_NAME }
    
    # First, try to find assets in the resources build phase (traditional projects)
    resources_phase = app_target.build_phases.find { |p| p.is_a?(Xcodeproj::Project::Object::PBXResourcesBuildPhase) }
    if resources_phase && preview_resources
      resources_phase.files.each do |build_file|
        file_ref = build_file.file_ref
        next unless file_ref
        
        if file_ref.path&.end_with?('.xcassets')
          preview_resources.add_file_reference(file_ref)
          log "Added asset catalog from resources: #{file_ref.path}"
        end
      end
    end
    
    # For Xcode 16+ projects with PBXFileSystemSynchronizedRootGroup,
    # find assets on disk in the source file's directory
    if @source_file
      source_dir = File.dirname(@source_file)
      project_dir = File.dirname(@project_path)
      
      # Look for .xcassets in the source directory and parent directories
      search_dirs = [source_dir]
      
      # Also check common locations relative to project
      app_target_dirs = @project.main_group.children.select do |c|
        c.is_a?(Xcodeproj::Project::Object::PBXFileSystemSynchronizedRootGroup) ||
        (c.respond_to?(:path) && c.path == app_target.name)
      end
      
      app_target_dirs.each do |dir|
        if dir.respond_to?(:path) && dir.path
          full_path = File.join(project_dir, dir.path)
          search_dirs << full_path if File.directory?(full_path)
        end
      end
      
      # Search for .xcassets directories
      search_dirs.uniq.each do |dir|
        next unless File.directory?(dir)
        
        Dir.glob(File.join(dir, '**', '*.xcassets')).each do |assets_path|
          next unless File.directory?(assets_path)
          
          # Copy the entire .xcassets directory to preview dir
          dest_name = File.basename(assets_path)
          dest_path = File.join(@preview_dir, dest_name)
          
          unless File.exist?(dest_path)
            FileUtils.cp_r(assets_path, dest_path)
            log "Copied asset catalog: #{dest_name}"
            
            # Add to project if we have preview_group and preview_resources
            if preview_group && preview_resources
              ref = preview_group.new_file(dest_path)
              preview_resources.add_file_reference(ref)
              log "Added asset catalog to target: #{dest_name}"
            end
          end
        end
      end
    end
  end
  
  def add_resource_bundles
    return if @dep_targets.nil? || @dep_targets.empty?
    
    # Collect all dependency names (including transitive)
    all_dep_names = Set.new
    queue = @dep_targets.dup
    
    while !queue.empty?
      t = queue.shift
      next if t.nil? || all_dep_names.include?(t.name)
      all_dep_names << t.name
      
      t.dependencies.each do |dep_ref|
        queue << dep_ref.target if dep_ref.target
      end
    end
    
    # Find bundle targets
    project_name = File.basename(@project_path, '.xcodeproj')
    bundle_targets = []
    
    @project.targets.each do |t|
      next unless t.product_type == 'com.apple.product-type.bundle'
      
      all_dep_names.each do |dep_name|
        # Check various naming conventions
        patterns = [
          "#{project_name}_#{dep_name}",  # Tuist convention
          "#{dep_name}_Resources",         # Generic _Resources suffix
          "#{dep_name}Resources",          # Generic Resources suffix
          dep_name                         # Direct match
        ]
        
        if patterns.any? { |p| t.name == p }
          bundle_targets << t unless bundle_targets.include?(t)
          break
        end
      end
    end
    
    return if bundle_targets.empty?
    
    log "Found #{bundle_targets.length} resource bundles"
    
    # Get resources build phase
    copy_phase = @target.build_phases.find { |p| p.is_a?(Xcodeproj::Project::Object::PBXResourcesBuildPhase) }
    
    bundle_targets.each do |bundle|
      @target.add_dependency(bundle)
      if bundle.product_reference && copy_phase
        copy_phase.add_file_reference(bundle.product_reference)
        log "Added resource bundle: #{bundle.name}"
      end
    end
  end
  
  def create_scheme
    scheme = Xcodeproj::XCScheme.new
    scheme.add_build_target(@target)
    scheme.set_launch_target(@target)
    
    scheme_dir = File.join(@project_path, 'xcshareddata', 'xcschemes')
    FileUtils.mkdir_p(scheme_dir)
    scheme.save_as(@project_path, PREVIEW_TARGET_NAME)
    log "Created scheme: #{PREVIEW_TARGET_NAME}"
  end
end

# Parse arguments
options = {}

if ARGV.include?('--json')
  # Read JSON from stdin
  input = JSON.parse(STDIN.read, symbolize_names: true)
  options = input
else
  # Parse command line arguments
  i = 0
  while i < ARGV.length
    case ARGV[i]
    when '--project'
      options[:project_path] = ARGV[i + 1]
      i += 2
    when '--preview-dir'
      options[:preview_dir] = ARGV[i + 1]
      i += 2
    when '--module'
      options[:module_name] = ARGV[i + 1]
      i += 2
    when '--imports'
      options[:imports] = ARGV[i + 1].split(',').map(&:strip)
      i += 2
    when '--deployment-target'
      options[:deployment_target] = ARGV[i + 1]
      i += 2
    when '--verbose', '-v'
      options[:verbose] = true
      i += 1
    when '--source-file'
      options[:source_file] = ARGV[i + 1]
      i += 2
    else
      i += 1
    end
  end
end

# Validate required options
unless options[:project_path]
  puts JSON.generate({ success: false, error: 'Missing required --project argument' })
  exit 1
end

unless File.exist?(options[:project_path])
  puts JSON.generate({ success: false, error: "Project not found: #{options[:project_path]}" })
  exit 1
end

# Run injection
injector = PreviewTargetInjector.new(options)
result = injector.run
puts JSON.generate(result)
exit(result[:success] ? 0 : 1)
