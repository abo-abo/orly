;;; orly.el --- Additional Org-mode link types and completion for them  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  Oleh Krehel

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
;; Finally, we can set up the links to clickable in e.g. HTML export. For
;; instance, man: links can point to https://linux.die.net/man/.
;; And el: links can point to https://git.savannah.gnu.org/cgit/emacs.git/tree/
;; or to https://github.com/emacs-mirror/emacs/.

;;; Code:

(require 'org)
(require 'counsel)
(require 'dabbrev)

(defgroup orly nil
  "Additional `org-mode' links and completion."
  :group 'convenience
  :prefix "orly-")

(defun orly-setup-links ()
  (org-link-set-parameters "el" :follow #'counsel--find-symbol :export #'orly--el-export)
  (org-link-set-parameters "man" :follow #'man :export #'orly--man-export)
  (org-link-set-parameters "pdf" :follow #'orly--open-pdf)
  (org-link-set-parameters "exe" :follow #'orly-describe-executable))

(orly-setup-links)

(defvar orly-completion-functions '(orly-completion-contacts
                                    orly-completion-symbols
                                    orly-completion-properties
                                    orly-completion-elisp
                                    orly-completion-executables
                                    orly-completion-filesystem
                                    orly-completion-refs
                                    pcomplete-completions-at-point
                                    orly-completion-dabbrev))

(defun orly-completion-function ()
  (run-hook-with-args-until-success 'orly-completion-functions))

(defun orly-setup-completion ()
  (cl-pushnew 'orly-completion-function completion-at-point-functions))

(defun orly--guess-cmd (files)
  (if current-prefix-arg
      (dired-read-shell-command "& on %s: " nil files)
    (let ((prog (dired-guess-default files)))
      (if (consp prog)
          (car prog)
        prog))))

(defun orly-autostart (file)
  (let ((cmd (orly--guess-cmd (list file))))
    (when cmd
      (orly-start cmd file))))

(defun orly-start (cmd &rest file-list)
  "Run CMD on FILE-LIST using nohup."
  (interactive
   (let* ((files (dired-get-marked-files t nil))
          (cmd (orly--guess-cmd files)))
     (if (cl-search (car files) cmd)
         (list cmd)
       (cons cmd files))))
  (let (fname)
    (if (and
         file-list
         (null (cdr file-list))
         (not (file-directory-p (setq fname (car file-list))))
         (file-executable-p fname)
         (string-match-p "^\\(#!\\|ELF\\)" (counsel--command "head" "-1" fname)))
        (let ((buf (compile (concat "./" (car file-list)) t)))
          (select-window (cl-find buf
                                  (window-list)
                                  :key #'window-buffer))
          (goto-char (point-max)))
      (start-process
       cmd nil shell-file-name
       shell-command-switch
       (concat
        (unless (string-match-p "|" cmd)
          "nohup 1>/dev/null 2>/dev/null ")
        cmd
        " "
        (mapconcat #'shell-quote-argument file-list " "))))))

(defun orly--open-pdf (path)
  "Example PATH to open on page 10: pdf:~/Downloads/test.pdf#10."
  (let ((cmd (if (string-match "\\`\\(.*\\)#\\(.*\\)\\'" path)
                 (let ((fname (match-string 1 path))
                       (page (match-string 2 path)))
                   (format "evince -p %s %s" page fname))
               path)))
    (orly-start cmd)))

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

(defcustom orly-el-web-address-function #'orly-el-web-address-github
  "Which website to use for pointing to Emacs sources."
  :type '(choice
          (const :tag "Github" orly-el-web-address-github)
          (const :tag "Savannah" orly-el-web-address-savannah)))

(defun orly-el-web-address-github (file line)
  (let ((base "https://github.com/emacs-mirror/emacs/blob/")
        (tag "emacs-26.3"))
    (concat
     base tag
     "/" file
     "#L" line)))

(defun orly-el-web-address-savannah (file line)
  (let ((base "https://git.savannah.gnu.org/cgit/emacs.git/tree/")
        (branch "emacs-26"))
    (concat
     base file
     "?h=" branch
     "#n" line)))

(defun orly--el-export (path _desc format)
  "Export the link to a built-in Emacs function or variable."
  (cond
    ((eq format 'html)
     (let* ((symbol (intern-soft path))
            (def
             (condition-case nil
                 (cond ((fboundp symbol)
                        (find-definition-noselect symbol nil))
                       ((boundp symbol)
                        (find-definition-noselect symbol 'defvar))
                       (t
                        nil))
               (error nil))))
       (if (null def)
           (format "<code>%s</code>" path)
         (or (let (file line)
               (with-current-buffer (car def)
                 (when (string-match "/\\(\\(?:src\\|lisp\\)/.*\\)\\'" buffer-file-name)
                   (setq file (match-string 1 buffer-file-name))
                   (setq line (number-to-string (line-number-at-pos (cdr def))))
                   (format "<a href=%S>%s</a>"
                           (funcall orly-el-web-address-function file line)
                           path))))
             (format "<code>%s</code>" path)))))
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

(defvar orly-contacts-file
  (if (featurep 'plain-org-wiki)
      (cdr (assoc "contacts" (plain-org-wiki-files)))
    nil)
  "Org file name where the contacts are second level outlines.")

(defun orly-to-nickname (name)
  (concat "@" (replace-regexp-in-string " " "_" (downcase name))))

(defun orly-read-contacts-file ()
  (let (res)
    (with-current-buffer (find-file-noselect orly-contacts-file)
      (save-match-data
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\*\\* \\([A-Za-z ]+\\)$" nil t)
            (let* ((name (match-string-no-properties 1))
                   (nick (orly-to-nickname name)))
              (push nick res))))))
    (nreverse res)))

(defvar orly-contacts nil)

(defun orly-completion-contacts ()
  (when (looking-back "@[a-z_]*" (line-beginning-position))
    (let ((cands (or orly-contacts
                     (setq orly-contacts (orly-read-contacts-file)))))
      (list
       (match-beginning 0) (match-end 0)
       (all-completions (match-string-no-properties 0) cands)))))

(defun orly-completion-properties ()
  (cond ((looking-back "^#[^ ]*" (line-beginning-position))
         (list (match-beginning 0)
               (match-end 0)
               (all-completions (match-string 0)
                                '("#+title: "
                                  "#+setupfile: "
                                  "#+property: "
                                  "#+include: "
                                  "#+call: "))))
        ((looking-back "^#\\+property: \\(.*\\)" (line-beginning-position))
         (list (match-beginning 1)
               (match-end 1)
               '("header-args ")))))

(defun orly-completion-elisp ()
  (when (looking-back "el:\\([a-zA-Z._-0-9]*\\)" (line-beginning-position))
    (list (match-beginning 1) (match-end 1)
          (all-completions
           (match-string-no-properties 1)
           obarray))))

(defun orly-completion-executables ()
  (when (looking-back "exe:\\([a-zA-Z._-0-9]*\\)" (line-beginning-position))
    (list (match-beginning 1) (match-end 1)
          (all-completions
           (match-string-no-properties 1)
           (orly-executables)))))

(defun orly-completion-filesystem ()
  (require 'ffap)
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

(defun orly-executables ()
  "Return the list of executables in `exec-path'."
  (let ((paths (cdr (reverse exec-path)))
        path
        completions)
    (while (setq path (pop paths))
      (let* ((dir (file-name-as-directory path))
             (comps (and (file-accessible-directory-p dir)
                         (file-name-all-completions "" dir)))
             comp)
        (while (setq comp (pop comps))
          (if (and (not (member comp completions))
                   (or (null shell-completion-execonly)
                       (file-executable-p (concat dir comp))))
              (push comp completions)))))
    completions))

(defun orly-describe-executable (exe)
  (shell-command
   (format
    "dpkg -S $(which %s) | awk '{gsub(\":\",\"\"); system(\"apt-cache show \" $1)}'"
    exe)))

(provide 'orly)
;;; orly.el ends here
