;;; orly.el --- Additional Org-mode link types and completion for them  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/orly
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (counsel "0.11.0"))
;; Keywords: convenience

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

;; As an example, you can make use of Elisp links, e.g. el:org-capture. Clicking
;; the link will open the definition of corresponding function / variable.  This
;; is useful when writing wikis / documentation, because you no longer have to
;; quote a part of the docstring: instead, the full docstring is just a click
;; away. man:ls is a similarly useful link type.
;;
;; One other nice feature is completion: after the link type is entered, press
;; "C-M-i" (`complete-symbol') to complete further.
;; To enable completion:
;;
;;     (add-hook 'org-mode-hook 'orly-setup-completion)
;;
;; Finally, we can set up the links to clickable in e.g. HTML
;; export. For instance, Man links can point to https://linux.die.net/man/.

;;; Code:

(require 'org)
(require 'counsel)

(defun orly-setup-links ()
  (org-link-set-parameters "el" :follow #'counsel--find-symbol :export #'orly--el-export)
  (org-link-set-parameters "man" :follow #'man :export #'orly--man-export))

(orly-setup-links)

(defun orly-setup-completion ()
  (setq completion-at-point-functions
        (delete-dups
         (append '(orly-completion-symbols
                   orly-completion-elisp
                   orly-completion-filesystem
                   orly-completion-refs
                   orly-completion-dabbrev)
                 completion-at-point-functions))))

(defun orly--man-export (path _desc format)
  "Export the link to a manpage."
  (let* ((full-names
          (ivy--re-filter (concat path "(")
                          (all-completions path 'Man-completion-table)))
         (full-name (if (= 1 (length full-names))
                        (substring-no-properties (car full-names))
                      path))
         (group (and (string-match "(\\([^()]+\\))" full-name)
                     (match-string 1 full-name))))
    (cond
      ((eq format 'html) (format "<a href=%S>%s</a>"
                                 (format "https://linux.die.net/man/%s/%s" group path)
                                 full-name))
      ((eq format 'latex) (format "\\textit{%s}" full-name))
      (t path))))

(defun orly--el-export (path _desc format)
  "Export the link to a built-in Emacs function or variable."
  (cond
    ((eq format 'html)
     (let* ((symbol (intern-soft path))
            (def
             (cond ((fboundp symbol)
                    (find-definition-noselect symbol nil))
                   ((boundp symbol)
                    (find-definition-noselect symbol 'defvar))
                   (t
                    nil))))
       (if (null def)
           path
         (let ((base "https://github.com/emacs-mirror/emacs/blob/")
               (tag "emacs-26.2")
               file line)
           (with-current-buffer (car def)
             (if (string-match "/\\(\\(?:src\\|lisp\\)/.*\\)\\'" buffer-file-name)
                 (setq file (match-string 1 buffer-file-name))
               (error "Could not find definiton: %S" symbol))
             (setq line (number-to-string (line-number-at-pos (cdr def)))))
           (format "<a href=%S>%s</a>"
                   (concat
                    base
                    tag
                    "/"
                    file
                    "#L"
                    line)
                   path)))))
    (t path)))

(defun orly-completion-symbols ()
  (when (looking-back "=[a-zA-Z]*" (line-beginning-position))
    (let (cands)
      (save-match-data
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "=\\([a-zA-Z._-]+\\)=" nil t)
            (cl-pushnew (match-string-no-properties 0) cands :test 'equal))
          cands))
      (when cands
        (list (match-beginning 0) (match-end 0) cands)))))

(defun orly-completion-elisp ()
  (when (looking-back "el:\\([a-zA-Z._-0-9]*\\)" (line-beginning-position))
    (list (match-beginning 1) (match-end 1)
          (all-completions
           (match-string-no-properties 1)
           obarray))))

(defun orly-completion-filesystem ()
  (let (path)
    (when (and (setq path (ffap-string-at-point))
               (not (string= path "")))
      (when (string-match "\\`file:\\(.*\\)\\'" path)
        (setq path (match-string 1 path)))
      (let ((compl (all-completions path #'read-file-name-internal)))
        (when compl
          (let* ((str (car compl))
                 (offset
                  (let ((i 0)
                        (len (length str)))
                    (while (and (< i len)
                                (equal (get-text-property i 'face str)
                                       'completions-common-part))
                      (cl-incf i))
                    i)))
            (list (- (point) offset) (point) compl)))))))

(defun orly-completion-refs ()
  (when (looking-back "\\\\\\(?:ref\\|label\\){\\([^\n{}]\\)*" (line-beginning-position))
    (let (cands beg end)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\label{\\([^}]+\\)}" nil t)
          (push (match-string-no-properties 1) cands)))
      (save-excursion
        (up-list)
        (setq end (1- (point)))
        (backward-list)
        (setq beg (1+ (point))))
      (list beg end
            (delete (buffer-substring-no-properties beg end)
                    (nreverse cands))))))

(defun orly-completion-dabbrev ()
  (ignore-errors
    (require 'dabbrev)
    (dabbrev--reset-global-variables)
    (let* ((dabbrev-check-other-buffers nil)
           (dabbrev-check-all-buffers nil)
           (abbrev (dabbrev--abbrev-at-point))
           (beg (progn (search-backward abbrev) (point)))
           (end (progn (search-forward abbrev) (point)))
           (ignore-case-p (dabbrev--ignore-case-p abbrev))
           (list 'uninitialized)
           (table
            (lambda (s p a)
              (if (eq a 'metadata)
                  `(metadata (cycle-sort-function . ,#'identity)
                             (category . dabbrev))
                (when (eq list 'uninitialized)
                  (save-excursion
                    ;; New abbreviation to expand.
                    (setq dabbrev--last-abbreviation abbrev)
                    ;; Find all expansion
                    (let ((completion-list
                           (dabbrev--find-all-expansions abbrev ignore-case-p))
                          (completion-ignore-case ignore-case-p))
                      (or (consp completion-list)
                          (user-error "No dynamic expansion for \"%s\" found%s"
                                      abbrev
                                      (if dabbrev--check-other-buffers
                                          "" " in this-buffer")))
                      (setq list
                            (mapcar #'downcase completion-list)))))
                (complete-with-action a list s p)))))
      (list beg end (all-completions "" table)))))

(provide 'orly)
;;; orly.el ends here
