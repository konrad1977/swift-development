#!/usr/bin/env ruby
# frozen_string_literal: true

# preview-helper.rb - Helper utilities for SwiftUI preview
#
# Usage:
#   ruby preview-helper.rb find-simulator "iPhone 16 Pro"
#   ruby preview-helper.rb first-booted
#   ruby preview-helper.rb simulator-state <udid>
#   ruby preview-helper.rb extract-preview path/to/file.swift
#   ruby preview-helper.rb detect-module path/to/file.swift
#
# Based on Claude-XcodePreviews (MIT License)

require 'json'

def find_simulator(name)
  json = `xcrun simctl list devices available -j 2>/dev/null`
  return nil if json.empty?
  
  data = JSON.parse(json)
  
  data['devices'].each do |runtime, devices|
    next unless runtime.include?('iOS')
    devices.each do |device|
      if device['name'] == name && device['isAvailable']
        puts device['udid']
        return
      end
    end
  end
  
  # Not found, try partial match
  data['devices'].each do |runtime, devices|
    next unless runtime.include?('iOS')
    devices.each do |device|
      if device['name'].include?(name) && device['isAvailable']
        puts device['udid']
        return
      end
    end
  end
  
  exit 1
end

def first_booted_simulator
  json = `xcrun simctl list devices booted -j 2>/dev/null`
  return nil if json.empty?
  
  data = JSON.parse(json)
  
  data['devices'].each do |_, devices|
    devices.each do |device|
      if device['state'] == 'Booted'
        puts device['udid']
        return
      end
    end
  end
  
  exit 1
end

def simulator_state(udid)
  json = `xcrun simctl list devices -j 2>/dev/null`
  data = JSON.parse(json)
  
  data['devices'].each do |_, devices|
    devices.each do |device|
      if device['udid'] == udid
        puts device['state']
        return
      end
    end
  end
  
  puts 'Unknown'
end

def simulator_info(udid)
  json = `xcrun simctl list devices -j 2>/dev/null`
  data = JSON.parse(json)
  
  data['devices'].each do |_, devices|
    devices.each do |device|
      if device['udid'] == udid
        puts "#{device['name']} (#{device['state']})"
        return
      end
    end
  end
  
  puts 'Unknown simulator'
end

def extract_preview(file_path)
  unless File.exist?(file_path)
    puts 'Text("File not found")'
    return
  end
  
  content = File.read(file_path)
  
  # Find #Preview and extract its body using brace counting
  # Match #Preview with optional parameters
  match = content.match(/#Preview(?:\s*\([^)]*\))?\s*\{/)
  
  unless match
    puts 'Text("No #Preview found")'
    return
  end
  
  # Start after the opening brace
  start_pos = match.end(0)
  brace_count = 1
  pos = start_pos
  in_string = false
  in_multiline_string = false
  
  while pos < content.length && brace_count > 0
    char = content[pos]
    next_two = content[pos, 2]
    next_three = content[pos, 3]
    
    # Handle multi-line strings
    if !in_string && next_three == '"""'
      in_multiline_string = !in_multiline_string
      pos += 3
      next
    end
    
    # Handle single-line strings (only if not in multi-line)
    if !in_multiline_string && char == '"' && (pos == 0 || content[pos - 1] != '\\')
      in_string = !in_string
      pos += 1
      next
    end
    
    # Skip if in any kind of string
    if in_string || in_multiline_string
      pos += 1
      next
    end
    
    # Handle comments
    if next_two == '//'
      # Skip to end of line
      newline = content.index("\n", pos)
      pos = newline ? newline + 1 : content.length
      next
    end
    
    if next_two == '/*'
      # Skip to end of block comment
      end_comment = content.index('*/', pos + 2)
      pos = end_comment ? end_comment + 2 : content.length
      next
    end
    
    # Count braces
    if char == '{'
      brace_count += 1
    elsif char == '}'
      brace_count -= 1
    end
    
    pos += 1
  end
  
  if brace_count != 0
    puts 'Text("Malformed #Preview")'
    return
  end
  
  # Extract body (excluding final closing brace)
  body = content[start_pos...pos - 1]
  
  # Dedent the body
  lines = body.split("\n")
  
  # Remove leading/trailing empty lines
  lines.shift while lines.any? && lines.first.strip.empty?
  lines.pop while lines.any? && lines.last.strip.empty?
  
  return if lines.empty?
  
  # Find minimum indentation
  min_indent = lines
    .reject { |line| line.strip.empty? }
    .map { |line| line.length - line.lstrip.length }
    .min || 0
  
  # Dedent all lines
  if min_indent > 0
    lines = lines.map do |line|
      line.length >= min_indent ? line[min_indent..] : line
    end
  end
  
  puts lines.join("\n")
end

def detect_module(file_path)
  # Detect module name from file path
  # Common patterns:
  # - Modules/ModuleName/Sources/...
  # - Sources/ModuleName/...
  # - Packages/PackageName/Sources/ModuleName/...
  
  patterns = [
    %r{Modules/([^/]+)/},
    %r{Sources/([^/]+)/},
    %r{Packages/[^/]+/Sources/([^/]+)/},
    %r{Features/([^/]+)/},
    %r{Frameworks/([^/]+)/}
  ]
  
  patterns.each do |pattern|
    match = file_path.match(pattern)
    if match
      puts match[1]
      return
    end
  end
  
  # Fallback: try to get from directory structure
  # Look for directory that's a sibling to Sources
  dir = File.dirname(file_path)
  while dir != '/' && dir != '.'
    parent = File.dirname(dir)
    basename = File.basename(dir)
    
    # If parent contains Package.swift, this might be the module
    if File.exist?(File.join(parent, 'Package.swift'))
      if basename != 'Sources' && basename != 'Tests'
        puts basename
        return
      end
    end
    
    dir = parent
  end
  
  # No module detected
  exit 1
end

def list_previews(file_path)
  unless File.exist?(file_path)
    puts JSON.generate({ success: false, error: 'File not found' })
    return
  end
  
  content = File.read(file_path)
  previews = []
  
  # Find all #Preview blocks
  content.scan(/#Preview(?:\s*\("([^"]+)"\)|\s*\(([^)]*)\))?\s*\{/) do |match|
    name = match[0] || 'Preview'
    previews << { name: name, index: previews.length }
  end
  
  # Also check for PreviewProvider
  if content.match(/struct\s+(\w+)\s*:\s*PreviewProvider/)
    previews << { name: $1, type: 'PreviewProvider', index: previews.length }
  end
  
  puts JSON.generate({ success: true, previews: previews, count: previews.length })
end

# Main command dispatch
case ARGV[0]
when 'find-simulator'
  find_simulator(ARGV[1])
when 'first-booted'
  first_booted_simulator
when 'simulator-state'
  simulator_state(ARGV[1])
when 'simulator-info'
  simulator_info(ARGV[1])
when 'extract-preview'
  extract_preview(ARGV[1])
when 'detect-module'
  detect_module(ARGV[1])
when 'list-previews'
  list_previews(ARGV[1])
else
  STDERR.puts "Usage: preview-helper.rb <command> [args]"
  STDERR.puts "Commands:"
  STDERR.puts "  find-simulator <name>   - Find simulator UDID by name"
  STDERR.puts "  first-booted            - Get first booted simulator UDID"
  STDERR.puts "  simulator-state <udid>  - Get simulator state"
  STDERR.puts "  simulator-info <udid>   - Get simulator name and state"
  STDERR.puts "  extract-preview <file>  - Extract #Preview body from Swift file"
  STDERR.puts "  detect-module <file>    - Detect module name from file path"
  STDERR.puts "  list-previews <file>    - List all previews in file"
  exit 1
end
