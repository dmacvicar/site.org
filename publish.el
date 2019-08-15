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
(require 'ert)
(require 's)
;; https://github.com/bastibe/org-static-blog

(defun post-get-metadata-from-filename (post-filepath)
  "Extract the `YYYY-MM-DD` date and base name from POST-FILE-PATH"
  (let* ((post-filename (file-name-nondirectory post-filepath))
    (result (s-match "^\\([0-9]\\{4\\}\\)\\-\\([0-9]\\{2\\}\\)\\-\\([0-9]\\{2\\}\\)\\-\\(.+\\)$" post-filename)))
    (if result
        (let ((date (parse-iso8601-time-string (format "%s-%s-%sT00:00:00" (nth 1 result) (nth 2 result) (nth 3 result))))
              (base-filename (file-name-sans-extension (nth 4 result))))
          (list :date date :base-filename base-filename))
      ())))

(ert-deftest post-get-metadata-from-filename ()
  "Tests extraction of metadata from `post-get-metadata-from-filename'"
  (let ((post-filename "2017-04-23-droid-chamaleon.org"))
    (should (equal "droid-chamaleon"
                   (plist-get (post-get-metadata-from-filename post-filename) :base-filename)))))

(defun post-get-metadata-from-frontmatter (post-filename)
  "Extract the `#+KEYWORD:` from POST-FILENAME. Keywords will be lowercase (#+TITLE: as :title)"
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents post-filename)
      (goto-char (point-min))
      (let ((metadata))
        (while
            (search-forward-regexp "^\\#\\+\\(.+\\):[ ]*\\(.+\\)$")
          (push
           (list (make-symbol (concat ":" (downcase (match-string 1)))) (match-string 2)) metadata))))))

;(post-get-metadata-from-frontmatter "posts/2017-04-23-droid-chamaleon.org")

(defun post-get-date (post-filepath)
  "Gets the post date"
  (plist-get (post-get-metadata-from-filename post-filepath) :date))

(ert-deftest post-get-date ()
  "Tests extract the date from the post file"
  (let ((post-filepath "posts/2017-04-23-droid-chamaleon.org"))
    (should (equal "2017-04-23" (format-time-string "%Y-%m-%d" (post-get-date post-filepath))))))

(defun post-target-path (post-filepath)
  "The path of the published post inside the publish dir"
  (format "%s/%s.html"
          (format-time-string "%Y/%m/%d" (post-get-date post-filepath))
          (plist-get (post-get-metadata-from-filename post-filepath) :base-filename)))

(ert-deftest post-target-path ()
  "Tests calculating the target path from the post file"
  (let ((post-filepath "posts/2017-04-23-droid-chamaleon.org"))
    (should (equal  "2017/04/23/droid-chamaleon.html" (post-target-path post-filepath)))))
  
(defvar duncan-website-html-head
  "<link rel='stylesheet' href='css/site.css' type='text/css'/>")

(defvar duncan-website-html-blog-head "")

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

;; deactivate cache as it does not take the publish.el file into account
(setq org-publish-cache nil)
(setq org-publish-use-timestamps-flag nil)
;; org-publish-timestamp-directory

(defun org-duncan-publish-to-html (plist filename pub-dir)
  "Wrapper publish function that publish blog entries in Jekyll's year/month/day/file.org path"
  (let* ((target-path (post-target-path filename))
         (pub-target-path (concat pub-dir target-path))
         (pub-target-dir (file-name-directory pub-target-path)))
    (unless (file-directory-p pub-target-dir)
      (make-directory pub-target-dir t))
    (message (format "Publishing %s to %s" filename pub-target-dir))
    (org-html-publish-to-html plist filename pub-target-dir)))

(setq org-publish-project-alist
      `(("org"
         :base-directory "./"
         :base-extension "org"
         :publishing-directory "./public"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc t

         :html-head ,duncan-website-html-head
         :html-preamble ,duncan-website-html-preamble
         :html-postamble ,duncan-website-html-postamble
         :html-validation-link nil
         :html-html5-fancy t
         :html-doctype "html5"
         :html-head-include-scripts nil
         :html-head-include-default-style nil)

        ("blog"
         :base-directory "./posts"
         :base-extension "org"
         :publishing-directory "./public/blog"
         :publishing-function org-duncan-publish-to-html
         :section-numbers nil
         :with-toc nil
         :html-head ,duncan-website-html-blog-head
         :html-head-extra
         "<link rel=\"alternate\" type=\"application/rss+xml\"
                href=\"http://duncan.codes/blog/blog.xml\"
                title=\"RSS feed\">"
         :html-preamble ,duncan-website-html-preamble
         :html-postamble ,duncan-website-html-postamble)

        ("tutorials"
         :base-directory "./tutorials"
         :base-extension "org"
         :recursive t
         :publishing-directory "./public/tutorials"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc t)
;         :html-head ,duncan-website-html-blog-head)
;         :html-preamble ,duncan-website-html-preamble
;         :html-postamble ,duncan-website-html-postamble)

        ("images"
         :base-directory "./assets/images"
         :recursive t
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "./public/assets/images"
         :publishing-function org-publish-attachment)

        ("js"
         :base-directory "./assets/js"
         :base-extension "js"
         :publishing-directory "./public/js"
         :publishing-function org-publish-attachment)

        ("css"
         :base-directory "./assets/css"
         :base-extension "css"
         :publishing-directory "./public/css"
         :publishing-function org-publish-attachment)

        ("rss"
         :base-directory "./posts"
         :base-extension "org"
         :publishing-directory "./public"
         :publishing-function (org-rss-publish-to-rss)
         :html-link-home "http://duncan.codes/"
         :html-link-use-abs-url t)

        ;("website" :components ("org" "blog" "images" "js" "css" "rss"))))
        ("website" :components ("blog"))))
(provide 'publish)
;;; publish.el ends here
