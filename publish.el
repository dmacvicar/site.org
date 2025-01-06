(require 'package)
(package-initialize)
(setq package-archives '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-refresh-contents)
(dolist (pkg '(weblorg htmlize string-inflection))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'weblorg)
(require 'string-inflection)

(if (string= (getenv "ENV") "production")
    (setq weblorg-default-url "https://mac-vicar.eu")
  (setq weblorg-default-url "http://localhost:8000"))

(weblorg-site
  :template-vars `(("org_version" . ,org-version)
                   ("emacs_version" . ,emacs-version)
                   ("site_owner" . ,user-full-name)
                   ("site_name" . "Duncan Mac-Vicar P. site")))

(defun custom/parse-org-file (file)
  "Custom wrapper for `weblorg--parse-org-file` to set the slug as the last folder name in the file path."
  (let ((parsed-data (weblorg--parse-org-file file)))
    (let* ((file-path (assoc-default "file" parsed-data))
           (slug (if file-path
                     (file-name-base (directory-file-name (file-name-directory file-path)))
                   "unknown-slug")))
      (setf (alist-get "slug" parsed-data) slug))
    parsed-data))

(weblorg-route
 :name "posts"
 :template "post.html"
 :input-pattern "posts/**/index.org"
 :output "public/posts/{{ slug }}/index.html"
 :url "//posts/{{ slug }}/"
 ; modifies slug to include the date
 :input-parser #'custom/parse-org-file)

 (weblorg-route
  :name "blog"
  :input-aggregate #'weblorg-input-aggregate-all-desc
  :template "blog.html"
  :input-pattern "posts/**/index.org"
  :output "public/posts/index.html"
  :url "//posts/"
  ; modifies slug to include the date
  :input-parser #'custom/parse-org-file)

(weblorg-route
 :name "feed"
 :input-pattern "posts/**/index.org"
 :input-aggregate #'weblorg-input-aggregate-all-desc
 :template "feed.xml"
 :output "public/posts/rss.xml"
 :url "/posts/rss.xml")

(weblorg-route
 :name "index"
 :input-pattern "index.org"
 :template "index.html"
 :output "public/index.html"
 :url "/")

(weblorg-route
 :name "pages"
 :template "page.html"
 :input-pattern "*.org"
 :input-exclude "index.org$"
 :output "public/{{ slug }}.html"
 :url "/{{ slug }}.html")

(weblorg-copy-static
  :output "public/{{ file }}"
  :url "/{{ file }}")

(weblorg-copy-static
 :name "post-images"
 :input-pattern "posts/**/images/*"
 :output "public/{{ file }}")

(let ((user-full-name "Duncan Mac-Vicar P.")
      (user-mail-address "duncan@mac-vicar.eu")
      (make-backup-files nil)
      (debug-on-error t)
      (org-src-fontify-natively t)
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
      (org-html-metadata-timestamp-format "%d %b. %Y")
      (org-time-stamp-custom-formats '("<%d %b %Y>" . "<%d %b %Y <%-l:%M %p>"))
      (org-display-custom-times t)
      (org-html-checkbox-type 'html)
      (org-html-html5-fancy t)
      (org-html-validation-link nil)
      (org-html-doctype "html5")
      ; add font-awesome icons as org entities \\faIconName
      (org-entities-user
       (let ((icons '("archive" "rss" "bookmark" "code" "github" "graduation-cap" "image")))
         (mapcar (lambda (icon)
                   (let ((name (concat "fa" (string-inflection-pascal-case-function icon))))
                     (list name
                           (concat "\\" name)
                           nil
                           (format "<i aria-hidden='true' class='fa fa-%s'></i>" icon)
                           "" "" "")))
                 icons)))
      (org-html-htmlize-output-type 'css)
      (org-safe-remote-resources '("https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup"))
      (org-publish-project-alist
       (list
        (list "assets"
              :base-directory "./"
              :exclude "theme.*\\|tutorials\\|public"
              :include '("CNAME" "LICENSE" "publish.el")
              :recursive t
              :base-extension (regexp-opt '("jpg" "gif" "png" "js" "svg" "css" "woff" "woff2"))
              :publishing-directory "./public"
              :publishing-function 'org-publish-attachment)
        (list "tutorials"
              :base-directory "./tutorials"
              :base-extension "org"
              :recursive t
              :publishing-directory "./public/tutorials"
              :publishing-function 'org-html-publish-to-html
              :section-numbers nil
              :with-toc t))))
  (add-to-list 'org-export-filter-timestamp-functions
               #'custom/filter-timestamp)
  (defun custom/filter-timestamp (trans back _comm)
    "Remove <> around time-stamps."
    (pcase back
      ((or `jekyll `html)
       (replace-regexp-in-string "&[lg]t;" "" trans))
      (`latex
       (replace-regexp-in-string "[<>]" "" trans))))
  (weblorg-export)
  (org-publish-all))



