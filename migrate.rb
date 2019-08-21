require 'fileutils'

BASE = '../dmacvicar.github.io'.freeze
MD_IMAGE_REGEX = /\!\[(.+)\]\(\s*\{\{\s*site\.baseurl\s*\}\}(.+)\)/.freeze
IMAGE_PATH_REGEX = /\/assets\/images\/posts\/(.+)\/(.+)/.freeze

Dir.glob(File.join(Dir.pwd, 'posts/**/*.org')).each do |entry|
  content = File.read(entry)
  content.to_enum(:scan, MD_IMAGE_REGEX).map { Regexp.last_match }.each do |data| 
    asset = data[2]
    # /assets/images/posts/2016-03-16-susemanager-3-backstage/minion-clients-2.png
    puts asset
    IMAGE_PATH_REGEX.match(asset) do |asset_data|
      post_id = asset_data[1]
      image = asset_data[2]
      img_dir = File.join(Dir.pwd, 'posts', post_id, "images")
      FileUtils.mkdir_p(img_dir)
      img_path = File.join(img_dir, image)
      puts "* " + File.join(BASE, asset) + " -> " + File.join(img_dir, image)
      FileUtils.cp File.join(BASE, asset), img_path
      content.gsub!(IMAGE_PATH_REGEX, '\2')
      content.gsub!(MD_IMAGE_REGEX, '[[file:images/\2]]')
      puts content
      File.write(entry, content)
    end
  end
end
