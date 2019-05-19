;;; orly.el --- Additional Org-mode link types and completion for them  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
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

;;; Code:

(require 'org)
(require 'counsel)

(let ((inhibit-message t))
  (org-add-link-type "el" 'counsel--find-symbol)
  (org-add-link-type "man" 'man))

(defun orly-completion-symbols ()
  (when (looking-back "=[a-zA-Z]*")
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
  (when (looking-back "el:\\([a-zA-Z._-0-9]*\\)")
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
  (when (looking-back "\\\\\\(?:ref\\|label\\){\\([^\n{}]\\)*")
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
                    ;;--------------------------------
                    ;; New abbreviation to expand.
                    ;;--------------------------------
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

(defun orly-setup-completion ()
  (setq completion-at-point-functions
        (delete-dups
         (append '(orly-completion-symbols
                   orly-completion-elisp
                   orly-completion-filesystem
                   orly-completion-refs
                   orly-completion-dabbrev)
                 completion-at-point-functions))))

(provide 'orly)
;;; orly.el ends here
