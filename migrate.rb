require 'fileutils'

BASE = '../dmacvicar.github.io'.freeze
MD_IMAGE_REGEX = /\!\[(.*)\]\(\s*\{\{\s*site\.baseurl\s*\}\}(.+)\)/.freeze

Dir.glob(File.join(Dir.pwd, 'posts/**/*.org')).each do |entry|
  next unless File.basename(entry) == 'index.org'
  post_id = File.basename(File.dirname(entry))
  content = File.read(entry)
  puts "Analyzing #{entry} (#{post_id})"
  content.to_enum(:scan, MD_IMAGE_REGEX).map { Regexp.last_match }.each do |data|
    caption = data[1].strip
    asset = data[2]
    img_src = File.join(BASE, asset)
    img_target_dir = File.join(Dir.pwd, 'posts', post_id, "images")
    img_target_path = File.join(img_target_dir, File.basename(img_src))

    puts "#{img_src} -> #{img_target_path}"

    replace_regex = /#{Regexp.escape(data[0])}/

    img_org_markup = "[[file:images/#{File.basename(img_target_path)}]]"
    puts "Replace #{replace_regex}"
    if caption != ""
      img_org_markup = "#+ATTR_HTML: :alt #{caption}\n#{img_org_markup}"
    end
    content.gsub!(replace_regex, img_org_markup)
    File.write(entry, content)
  end
end
