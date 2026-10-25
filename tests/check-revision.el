;;; check-revision.el --- Run: emacs -Q --batch --script tests/check-revision.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

;; Load only the helper, without installing packages or publishing the site.
(with-temp-buffer
  (insert-file-contents "publish.el")
  (goto-char (point-min))
  (re-search-forward "^(defun get-git-short-hash")
  (beginning-of-line)
  (eval (read (current-buffer))))

(let ((process-environment (copy-sequence process-environment)))
  (setenv "GITHUB_SHA" "abcdef0123456789abcdef0123456789abcdef01")
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _) (error "CI must not invoke Git"))))
    (cl-assert (equal (get-git-short-hash) "abcdef0")))
  (setenv "GITHUB_SHA" nil)
  (cl-letf (((symbol-function 'call-process)
             (lambda (program _input output _display &rest args)
               (cl-assert (equal program "git"))
               (cl-assert (equal args '("rev-parse" "--short" "HEAD")))
               (princ "1234567\n" output)
               0)))
    (cl-assert (equal (get-git-short-hash) "1234567"))))

(message "PASS: CI revision works without Git; local Git fallback is preserved")
