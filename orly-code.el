;;; orly-links.el --- Code links for Orly  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Oleh Krehel

;; Author: Oleh Krehel

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This adds links like [code:orly/orly.el/orly-setup-links/a309064]
;; to `org-mode'.
;;
;; Advantages over the existing [file:...] link:
;; * Repositores can be moved around without changing the link
;; * More readable links: use functions instead of line numbers
;; * Visit the link on remote if needed
;; * Completion for each part of the link

;;; Code:
(defvar orly-repos '((".cook.d" "~/.cook.d/"))
  "An alist of repository names to repository locations.
You can use `orly-read-repos-file' to initialize this variable.")

(defun orly-read-repos-file (org-fname)
  "Read `orly-repos' from ORG-FNAME.
Each repo should be a \"file:~/foo/bar\".
All lines that don't match this will be ignored."
  (with-current-buffer (find-file-noselect org-fname)
    (let ((res nil))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^file:\\(.*\\)$" nil t)
          (let ((fname (match-string-no-properties 1)))
            (push (list
                   (file-name-nondirectory (directory-file-name fname))
                   fname)
                  res))))
      (nreverse res))))

(defun orly-open-code-link (code-link)
  "Open CODE-LINK.
CODE-LINK is REPO/FNAME/FUN/REV.
The last 2 parts are optional."
  (let* ((parts (split-string code-link "/"))
         (repo (nth 0 parts))
         (local-repo (assoc repo orly-repos))
         (fname (if local-repo
                    (expand-file-name (nth 1 parts) (nth 1 local-repo))
                  (error "Could not find repo: %s" repo)))
         (rest (cddr parts)))
    (cl-case (length rest)
      (0
       (find-file fname))
      (1
       (find-file fname)
       (orly-find-function (nth 0 rest)))
      (2
       (let ((rev (nth 1 rest)))
         (switch-to-buffer (vc-find-revision fname rev))
         (orly-find-function (nth 0 rest)))))))

(cl-defgeneric orly-find-function (fun)
  "Find FUN in the current file."
  (goto-char (point-min))
  (search-forward fun nil t))

(cl-defmethod orly-find-function (fun &context (major-mode python-mode))
  (goto-char (point-min))
  (search-forward (concat "def " fun))
  (require 'lpy)
  (lpy-back-to-special))

(org-link-set-parameters "code" :follow #'orly-open-code-link)

(provide 'orly-links)
;;; orly-links.el ends here
