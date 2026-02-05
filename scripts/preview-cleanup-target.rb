#!/usr/bin/env ruby
# frozen_string_literal: true

# preview-cleanup-target.rb - Remove PreviewHost target from Xcode project
#
# Usage:
#   ruby preview-cleanup-target.rb --project /path/to/Project.xcodeproj
#
# Or:
#   ruby preview-cleanup-target.rb /path/to/Project.xcodeproj

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

PREVIEW_TARGET_NAME = 'PreviewHost'

def cleanup_project(project_path, verbose: false)
  unless File.exist?(project_path)
    return { success: false, error: "Project not found: #{project_path}" }
  end
  
  project = Xcodeproj::Project.open(project_path)
  removed_target = false
  removed_group = false
  removed_scheme = false
  
  # Remove target
  target = project.targets.find { |t| t.name == PREVIEW_TARGET_NAME }
  if target
    target.remove_from_project
    removed_target = true
    STDERR.puts "[cleanup] Removed target: #{PREVIEW_TARGET_NAME}" if verbose
  end
  
  # Remove group
  project.main_group.groups.each do |g|
    if g.name == PREVIEW_TARGET_NAME
      g.remove_from_project
      removed_group = true
      STDERR.puts "[cleanup] Removed group: #{PREVIEW_TARGET_NAME}" if verbose
      break
    end
  end
  
  project.save if removed_target || removed_group
  
  # Remove scheme
  scheme_path = File.join(project_path, 'xcshareddata', 'xcschemes', "#{PREVIEW_TARGET_NAME}.xcscheme")
  if File.exist?(scheme_path)
    FileUtils.rm_f(scheme_path)
    removed_scheme = true
    STDERR.puts "[cleanup] Removed scheme: #{PREVIEW_TARGET_NAME}" if verbose
  end
  
  {
    success: true,
    removed_target: removed_target,
    removed_group: removed_group,
    removed_scheme: removed_scheme
  }
rescue StandardError => e
  { success: false, error: e.message }
end

# Parse arguments
project_path = nil
verbose = false

ARGV.each_with_index do |arg, i|
  case arg
  when '--project'
    project_path = ARGV[i + 1]
  when '--verbose', '-v'
    verbose = true
  else
    # Bare argument (project path)
    project_path = arg if arg.end_with?('.xcodeproj') && project_path.nil?
  end
end

unless project_path
  puts JSON.generate({ success: false, error: 'No project path specified' })
  exit 1
end

result = cleanup_project(project_path, verbose: verbose)
puts JSON.generate(result)
exit(result[:success] ? 0 : 1)
