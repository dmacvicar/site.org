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
  "Formats the layout named NAME by reading a file from a directory."
  `(("en" ,(with-temp-buffer
             (insert-file-contents (expand-file-name (format "%s.html" name) "./layouts"))
             (buffer-string)))))

(defun duncan/org-publish-sitemap-latest-posts (title list)
  "Wrapper to skip TITLE and just use LIST (https://orgmode.org/manual/Sitemap.html)."
  (org-list-to-org list))

(defun duncan/org-publish-sitemap-archive (title list)
  "Wrapper to skip TITLE and just use LIST (https://orgmode.org/manual/Sitemap.html)."
  (org-list-to-org list))

(defun duncan/org-publish-sitemap-entry (entry style project)
  "Format sitemap ENTRY for PROJECT with the post date before the link, to generate a posts list.  STYLE is not used."
  (unless (equal entry "404.org")
    (format "%s [[file:%s][%s]]"
            (format-time-string "<%Y-%m-%d>" (org-publish-find-date entry project))
            entry
            (org-publish-find-title entry project))))

(defun duncan/org-html-timestamp (timestamp contents info)
  "We are not going to leak org mode silly <date> format when rendering TIMESTAMP to the world, aren't we?.  CONTENTS and INFO are passed down to org-html-timestamp."
  (let ((org-time-stamp-custom-formats
       '("%d %b %Y" . "%d %b %Y %H:%M"))
        (org-display-custom-times 't))
    (org-html-timestamp timestamp contents info)))

; We derive our own backend in order to override the timestamp format of the html backend
(org-export-define-derived-backend 'duncan/html 'html
  :translate-alist
  '((timestamp . duncan/org-html-timestamp)))

(defun duncan/post-get-metadata-from-frontmatter (post-filename key)
  "Extract the KEY as`#+KEY:` from POST-FILENAME."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents post-filename)
      (goto-char (point-min))
      (ignore-errors (search-forward-regexp (format "^\\#\\+%s\\:\s+\\(.+\\)$" key)))
      (match-string 1))))

(defun duncan/org-html-publish-generate-redirect (plist filename pub-dir)
  "Generate redirect files in PUB-DIR from the #+REDIRECT_FROM header in FILENAME, using PLIST."
  (let* ((redirect-from (duncan/post-get-metadata-from-frontmatter filename "REDIRECT_FROM"))
         (root (projectile-project-root))
         (pub-root (concat root "public"))
         (target-filepath (concat pub-root redirect-from))
         (target-filename (file-name-nondirectory target-filepath))
         (rel-filename (file-relative-name filename (file-name-directory target-filepath))))
    (when redirect-from
      (with-temp-buffer
                                        ;(insert-file-contents (concat root "layouts/redirect.html"))
        (insert (format "[[file:%s][Redirect]]" rel-filename))
        (make-directory (file-name-directory target-filepath) :parents)
        (org-export-to-file 'duncan/html target-filepath nil nil nil nil plist))
      (message (format "Create %s" target-filepath)))))

(defun duncan/org-html-publish-to-html (plist filename pub-dir)
  "Analog to org-html-publish-to-html using duncan/html backend.  PLIST, FILENAME and PUB-DIR are passed as is."
  (duncan/org-html-publish-generate-redirect plist filename pub-dir)
  (org-publish-org-to 'duncan/html filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension
				      "html"))
		      plist pub-dir))

(defun duncan/org-html-publish-post-to-html (plist filename pub-dir)
  "Wraps org-html-publish-to-html.  Append post date as subtitle to PLIST.  FILENAME and PUB-DIR are passed."
  (let ((project (cons 'blog plist)))
    (plist-put plist :subtitle
               (format-time-string "%b %d, %Y" (org-publish-find-date filename project)))
    (duncan/org-html-publish-to-html plist filename pub-dir)))

(defun duncan/org-rss-publish-to-rss (plist filename pub-dir)
  "Wrap org-rss-publish-to-rss with PLIST and PUB-DIR, publishing only when FILENAME is 'archive.org'."
  (if (equal "archive.org" (file-name-nondirectory filename))
      (org-rss-publish-to-rss plist filename pub-dir)))

; Project definition
(defvar duncan--publish-project-alist
      (list
       (list "blog"
             :base-directory "./posts"
             :exclude (regexp-opt '("posts.org" "archive.org"))
             :base-extension "org"
             :recursive t
             :publishing-directory (expand-file-name "public/posts" (projectile-project-root))
             :publishing-function 'duncan/org-html-publish-post-to-html
             :section-numbers nil
             :with-toc nil
             :html-head duncan-website-html-head
             :html-preamble t
             :html-preamble-format (duncan--layout-format 'preamble)
             :html-postamble t
             :html-postamble-format (duncan--layout-format 'postamble)
             :html-head-include-scripts nil
             :html-htmlized-css-url "/css/site.css"
             :html-head-include-default-style nil
             :auto-sitemap t
             :sitemap-filename "posts.org"
             :sitemap-style 'list
             :sitemap-title nil
             :sitemap-sort-files 'anti-chronologically
             :sitemap-function 'duncan/org-publish-sitemap-latest-posts
             :sitemap-format-entry 'duncan/org-publish-sitemap-entry)

        (list "archive-rss"
              :base-directory "./posts"
              :recursive t
              :exclude (regexp-opt '("posts.org" "archive.org"))
              :base-extension "org"
              :publishing-directory "./public"
              :publishing-function 'duncan/org-rss-publish-to-rss
              :html-link-home "http://duncan.codes/"
              :html-link-use-abs-url t
              :auto-sitemap t
              :sitemap-style 'list
              :sitemap-filename "archive.org"
              :sitemap-sort-files 'anti-chronologically
              :sitemap-function 'duncan/org-publish-sitemap-archive
              :sitemap-format-entry 'duncan/org-publish-sitemap-entry)

        (list "website"
              :base-directory "./"
              :include '("posts/archive.org")
              :base-extension "org"
              :publishing-directory (expand-file-name "public" (projectile-project-root))
              :publishing-function 'duncan/org-html-publish-to-html
              :section-numbers nil
              :html-head duncan-website-html-head
              :html-preamble t
              :html-preamble-format (duncan--layout-format 'preamble)
              :html-postamble t
              :html-postamble-format (duncan--layout-format 'postamble)
              :html-validation-link nil
              :html-htmlized-css-url "/css/site.css"
              :html-head-include-scripts nil
              :html-head-include-default-style nil)
        (list "tutorials"
              :base-directory "./tutorials"
              :base-extension "org"
              :recursive nil
              :publishing-directory "./public/tutorials"
              :publishing-function 'org-html-publish-to-html
              :section-numbers nil
              :with-toc t)
        (list "assets"
              :base-directory "./"
              :exclude (regexp-opt '("assets" "public"))
              :recursive t
              :base-extension "jpg\\|gif\\|png\\|js\\|css"
              :publishing-directory "./public"
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

(provide 'publish)
;;; publish.el ends here

