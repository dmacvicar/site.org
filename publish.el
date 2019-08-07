;; publish.el --- Publish org-mode project on Gitlab Pages
;; Author: Rasmus

;;; Commentary:
;; This script will convert the org-mode files in this directory into
;; html.

;;; Code:

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)
(package-install 'org-plus-contrib)
(package-install 'htmlize)

(require 'org)
(require 'ox-rss)
(require 'ox-publish)


(defvar duncan-website-html-head
"<link href='http://fonts.googleapis.com/css?family=Libre+Baskerville:400,400italic' rel='stylesheet' type='text/css'>
<link rel='stylesheet' href='css/site.css' type='text/css'/>")

(defvar duncan-website-html-blog-head
"<link href='http://fonts.googleapis.com/css?family=Libre+Baskerville:400,400italic' rel='stylesheet' type='text/css'>
<link rel='stylesheet' href='../css/site.css' type='text/css'/>")

(defvar duncan-website-html-preamble 
  "<div class='nav'>
<ul>
<li><a href='/'>Home</a></li>
<li><a href='/blog/index.html'>Blog</a></li>
<li><a href='http://github.com/dmacvicar'>GitHub</a></li>
<li><a href='http://twitter.com/dmacvicar'>Twitter</a></li>
<li><a href='/contact.html'>Contact</a></li>
</ul>
</div>")

(defvar duncan-website-html-postamble 
  "<div class='footer'>
Copyright 2019 %a (%v HTML).<br>
Last updated %C. <br>
Built with %c.
</div>")

(setq org-publish-project-alist
      `(("org"
         :base-directory "."
         :base-extension "org"
         :publishing-directory "./public"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :html-head ,duncan-website-html-head
         :html-preamble ,duncan-website-html-preamble
         :html-postamble ,duncan-website-html-postamble)

        ("blog"
         :base-directory "./blog"
         :base-extension "org"
         :publishing-directory "./public/blog"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :html-head ,duncan-website-html-blog-head
         :html-head-extra
         "<link rel=\"alternate\" type=\"application/rss+xml\"
                href=\"http://duncan.codes/blog/blog.xml\"
                title=\"RSS feed\">"
         :html-preamble ,duncan-website-html-preamble
         :html-postamble ,duncan-website-html-postamble)

        ("images"
         :base-directory "./assets/images"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "./public/images"
         :publishing-function org-publish-attachment)

        ("js"
         :base-directory "./assets/js"
         :base-extension "js"
         :publishing-directory "./public/js"
         :publishing-function org-publish-attachment)

        ("css"
         :base-directory "./assets/css"
         :base-extension "css"
         :publishing-directory "./public/css/"
         :publishing-function org-publish-attachment)

        ("rss"
         :base-directory "./blog"
         :base-extension "org"
         :publishing-directory "./blog"
         :publishing-function (org-rss-publish-to-rss)
         :html-link-home "http://duncan.codes/"
         :html-link-use-abs-url t)

        ("website" :components ("org" "blog" "images" "js" "css" "rss"))))


(provide 'publish)
;;; publish.el ends here
