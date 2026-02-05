#!/usr/bin/env ruby
# frozen_string_literal: true

# preview-spm-create.rb - Create temporary Xcode project for SPM package preview
#
# Usage:
#   ruby preview-spm-create.rb --package /path/to/Package.swift \
#                               --module ModuleName \
#                               --preview-dir /tmp/PreviewHost \
#                               --output /tmp/PreviewProject
#
# This creates a temporary Xcode project that depends on the local SPM package,
# allowing us to build and preview SwiftUI views from the package.

require 'json'
require 'fileutils'

begin
  require 'xcodeproj'
rescue LoadError
  gem_paths = [
    File.expand_path('~/.gem/ruby/4.0.0'),
    File.expand_path('~/.gem/ruby/3.3.0'),
    File.expand_path('~/.gem/ruby/3.2.0'),
    File.expand_path('~/.gem/ruby/3.1.0'),
    File.expand_path('~/.gem/ruby/3.0.0'),
    File.expand_path('~/.gem/ruby/2.6.0')
  ]
  
  loaded = false
  gem_paths.each do |path|
    gems_dir = File.join(path, 'gems')
    next unless File.directory?(gems_dir)
    
    Dir.glob(File.join(gems_dir, '*', 'lib')).each do |lib|
      $LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
    end
    
    begin
      require 'xcodeproj'
      loaded = true
      break
    rescue LoadError
      next
    end
  end
  
  unless loaded
    puts JSON.generate({ success: false, error: 'xcodeproj gem not found' })
    exit 1
  end
end

class SPMProjectCreator
  PREVIEW_TARGET_NAME = 'PreviewHost'
  PREVIEW_BUNDLE_ID = 'com.swift-development.preview-spm'
  
  def initialize(options)
    @package_path = options[:package_path]
    @package_dir = File.dirname(@package_path)
    @module_name = options[:module_name]
    @preview_dir = options[:preview_dir]
    @output_dir = options[:output_dir]
    @deployment_target = options[:deployment_target] || '17.0'
    @verbose = options[:verbose] || false
  end
  
  def run
    log "Creating SPM preview project"
    log "Package: #{@package_path}"
    log "Module: #{@module_name}"
    
    # Parse Package.swift for deployment target
    detect_deployment_target
    
    # Create output directory
    FileUtils.mkdir_p(@output_dir)
    
    @project_path = File.join(@output_dir, 'PreviewHost.xcodeproj')
    
    # Create new project
    @project = Xcodeproj::Project.new(@project_path)
    
    # Create main group
    create_source_group
    
    # Create app target
    create_preview_target
    
    # Configure build settings
    configure_build_settings
    
    # Add local package reference
    add_package_reference
    
    # Save project
    @project.save
    log "Project saved: #{@project_path}"
    
    # Create scheme
    create_scheme
    
    {
      success: true,
      project_path: @project_path,
      scheme: PREVIEW_TARGET_NAME,
      deployment_target: @deployment_target,
      bundle_id: PREVIEW_BUNDLE_ID
    }
  rescue StandardError => e
    { success: false, error: e.message, backtrace: e.backtrace.first(5) }
  end
  
  private
  
  def log(msg)
    STDERR.puts "[preview-spm] #{msg}" if @verbose
  end
  
  def detect_deployment_target
    return unless File.exist?(@package_path)
    
    content = File.read(@package_path)
    
    # Look for .iOS(.v17) or similar
    match = content.match(/\.iOS\(\.v(\d+)/)
    if match
      @deployment_target = "#{match[1]}.0"
      log "Detected iOS deployment target: #{@deployment_target}"
    end
  end
  
  def create_source_group
    return unless @preview_dir && File.directory?(@preview_dir)
    
    @preview_group = @project.main_group.new_group(PREVIEW_TARGET_NAME, @preview_dir)
    
    # Add Swift files
    Dir.glob(File.join(@preview_dir, '*.swift')).each do |swift_file|
      @preview_group.new_file(swift_file)
    end
  end
  
  def create_preview_target
    @target = @project.new_target(:application, PREVIEW_TARGET_NAME, :ios, @deployment_target)
    
    # Add source files to target
    if @preview_group
      @preview_group.files.each do |file_ref|
        @target.source_build_phase.add_file_reference(file_ref) if file_ref.path.end_with?('.swift')
      end
    end
    
    log "Created target: #{PREVIEW_TARGET_NAME}"
  end
  
  def configure_build_settings
    @target.build_configurations.each do |config|
      config.build_settings['PRODUCT_BUNDLE_IDENTIFIER'] = PREVIEW_BUNDLE_ID
      config.build_settings['GENERATE_INFOPLIST_FILE'] = 'YES'
      config.build_settings['INFOPLIST_KEY_UIApplicationSceneManifest_Generation'] = 'YES'
      config.build_settings['INFOPLIST_KEY_UILaunchScreen_Generation'] = 'YES'
      config.build_settings['SWIFT_VERSION'] = '5.0'
      config.build_settings['CODE_SIGN_STYLE'] = 'Automatic'
      config.build_settings['IPHONEOS_DEPLOYMENT_TARGET'] = @deployment_target
    end
  end
  
  def add_package_reference
    # Create XCLocalSwiftPackageReference
    local_pkg = @project.new(Xcodeproj::Project::Object::XCLocalSwiftPackageReference)
    local_pkg.relative_path = @package_dir
    
    # Add to root object
    @project.root_object.package_references ||= []
    @project.root_object.package_references << local_pkg
    
    # Create product dependency
    pkg_product = @project.new(Xcodeproj::Project::Object::XCSwiftPackageProductDependency)
    pkg_product.product_name = @module_name
    pkg_product.package = local_pkg
    
    # Add to target's package product dependencies
    @target.package_product_dependencies ||= []
    @target.package_product_dependencies << pkg_product
    
    log "Added package dependency: #{@module_name}"
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
  input = JSON.parse(STDIN.read, symbolize_names: true)
  options = input
else
  i = 0
  while i < ARGV.length
    case ARGV[i]
    when '--package'
      options[:package_path] = ARGV[i + 1]
      i += 2
    when '--module'
      options[:module_name] = ARGV[i + 1]
      i += 2
    when '--preview-dir'
      options[:preview_dir] = ARGV[i + 1]
      i += 2
    when '--output'
      options[:output_dir] = ARGV[i + 1]
      i += 2
    when '--deployment-target'
      options[:deployment_target] = ARGV[i + 1]
      i += 2
    when '--verbose', '-v'
      options[:verbose] = true
      i += 1
    else
      i += 1
    end
  end
end

# Validate
unless options[:package_path]
  puts JSON.generate({ success: false, error: 'Missing --package argument' })
  exit 1
end

unless File.exist?(options[:package_path])
  puts JSON.generate({ success: false, error: "Package.swift not found: #{options[:package_path]}" })
  exit 1
end

unless options[:module_name]
  puts JSON.generate({ success: false, error: 'Missing --module argument' })
  exit 1
end

options[:output_dir] ||= File.join(Dir.tmpdir, "preview-spm-#{Process.pid}")

creator = SPMProjectCreator.new(options)
result = creator.run
puts JSON.generate(result)
exit(result[:success] ? 0 : 1)
