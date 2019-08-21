(require 'package)
(package-initialize)
(unless package-archive-contents
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents))
(dolist (pkg '(projectile org-plus-contrib htmlize))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'org)
(require 'ox-rss)
(require 'ox-publish)
(require 'ert)
(require 's)
(require 'projectile)

(defvar duncan-website-html-head
  "<link href='https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css' rel='stylesheet' integrity='sha256-k2/8zcNbxVIh5mnQ52A0r3a6jAgMGxFJFE2707UxGCk= sha512-ZV9KawG2Legkwp3nAlxLIVFudTauWuBpC10uEafMHYL0Sarrz5A7G79kXh5+5+woxQ5HM559XX2UZjMJ36Wplg==' crossorigin='anonymous'/>")
(defvar duncan-website-html-blog-head "")

(defun duncan--layout-format (name)
  "Format the layout named NAME"
  `(("en" ,(with-temp-buffer
             (insert-file-contents (expand-file-name (format "%s.html" name) "./layouts"))
             (buffer-string)))))

(defun duncan/org-publish-sitemap-latest-posts (title list)
  "Wrapper to skip title. See https://orgmode.org/manual/Sitemap.html"
  (org-list-to-org list))

(defun duncan/org-publish-sitemap-archive (title list)
  "Wrapper to skip title. See https://orgmode.org/manual/Sitemap.html"
  (org-list-to-org list))

(defun duncan/org-publish-sitemap-entry (entry style project)
  "Format for sitemap ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (unless (equal entry "404.org")
    (format "%s [[file:%s][%s]]"
            (format-time-string "<%Y-%m-%d>" (org-publish-find-date entry project))
            entry
            (org-publish-find-title entry project))))

(defun duncan/org-html-timestamp (timestamp contents info)
  "We are not going to leak org mode silly <date> format to the world, aren't we?"
  (let ((org-time-stamp-custom-formats
       '("%d %b %Y" . "%d %b %Y %H:%M"))
        (org-display-custom-times 't))
    (org-html-timestamp timestamp contents info)))

; We derive our own backend in order to override the timestamp format of the html backend
(org-export-define-derived-backend 'duncan/html 'html
  :translate-alist
  '((timestamp . duncan/org-html-timestamp)))

(defun duncan/org-html-publish-to-html (plist filename pub-dir)
  "Analog to org-html-publish-to-html for duncan/html backend"
  (org-publish-org-to 'duncan/html filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension
				      "html"))
		      plist pub-dir))

; Project definition
(defvar duncan--publish-project-alist
      (list
       (list "blog"
             :base-directory "./posts"
             :exclude (regexp-opt '("posts.org" "archive.org"))
             :base-extension "org"
             :publishing-directory (expand-file-name "public" (projectile-project-root))
             :publishing-function 'org-html-publish-to-html
             :section-numbers nil
             :with-toc nil
             :html-head duncan-website-html-head
             :html-preamble t
             :html-preamble-format (duncan--layout-format 'preamble)
             :html-postamble t
             :html-postamble-format (duncan--layout-format 'postamble)
             :html-head-include-scripts nil
             :html-htmlized-css-url "../css/site.css"
             :html-head-include-default-style nil
             :auto-sitemap t
             :sitemap-filename "posts.org"
             :sitemap-title nil
             :sitemap-sort-files 'anti-chronologically
             :sitemap-function 'duncan/org-publish-sitemap-latest-posts
             :sitemap-format-entry 'duncan/org-publish-sitemap-entry)

        (list "rss"
              :base-directory "./posts"
              :exclude (regexp-opt '("posts.org" "archive.org"))
              :base-extension "org"
              :publishing-directory "./public"
              :publishing-function 'org-rss-publish-to-rss
              :html-link-home "http://duncan.codes/"
              :html-link-use-abs-url t
              :auto-sitemap t
              :sitemap-filename "archive.org"
                                        ;:sitemap-sort-files 'anti-chronologically
              :sitemap-function 'duncan/org-publish-sitemap-archive)

        (list "website"
              :base-directory "./"
              :base-extension "org"
              :publishing-directory (expand-file-name "public" (projectile-project-root))
              :publishing-function 'duncan/org-html-publish-to-html
              :section-numbers nil
              :html-head duncan-website-html-head
              :html-htmlized-css-url "css/site.css"
              :html-preamble t
              :html-preamble-format (duncan--layout-format 'preamble)
              :html-postamble t
              :html-postamble-format (duncan--layout-format 'postamble)
              :html-validation-link nil
              :html-htmlized-css-url "css/site.css"
              :html-head-include-scripts nil
              :html-head-include-default-style nil)
        (list "tutorials"
              :base-directory "./tutorials"
              :base-extension "org"
              :recursive t
              :publishing-directory "./public/tutorials"
              :publishing-function 'org-html-publish-to-html
              :section-numbers nil
              :with-toc t)
        (list "images"
              :base-directory "./assets/images"
              :recursive t
              :base-extension "jpg\\|gif\\|png"
              :publishing-directory "./public/assets/images"
              :publishing-function 'org-publish-attachment)
        (list "js"
              :base-directory "./assets/js"
              :base-extension "js"
              :publishing-directory "./public/js"
         :publishing-function 'org-publish-attachment)
        (list "css"
              :base-directory "./css"
              :base-extension "css"
              :publishing-directory "./public/css"
              :publishing-function 'org-publish-attachment)))

; Our publishing definition
(defun duncan-publish-all ()
  "Publish the blog to HTML."
  (interactive)
  (let ((make-backup-files nil)
        (org-publish-project-alist       duncan--publish-project-alist)
        (org-publish-timestamp-directory "./.timestamps/")
        ;; deactivate cache as it does not take the publish.el file into account
        (org-publish-cache nil)
        (org-publish-use-timestamps-flag nil)
        (org-export-with-section-numbers nil)
        (org-export-with-smart-quotes    t)
        (org-export-with-toc             nil)
        (org-export-with-sub-superscripts '{})
        (org-html-divs '((preamble  "header" "preamble")
                         (content   "main"   "content")
                         (postamble "footer" "postamble")))
        (org-html-container-element         "section")
        (org-html-metadata-timestamp-format "%Y-%m-%d")
        (org-html-checkbox-type             'html)
        (org-html-html5-fancy               t)
        (org-html-validation-link           nil)
        (org-html-doctype                   "html5")
        (org-entities-user
         (quote
          (("faBookmark" "\\faBookmark" nil "<i aria-hidden='true' class='fa fa-bookmark'></i>" "" "" "")
           ("faCode" "\\faCode" nil "<i aria-hidden='true' class='fa fa-code'></i>" "" "" "")
           ("faGraduationCap" "\\faGraduationCap" nil "<i aria-hidden='true' class='fa fa-graduation-cap'></i>" "" "" ""))))
        (org-html-htmlize-output-type       'css))
    (org-publish-all)))
