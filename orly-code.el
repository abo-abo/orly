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

;; This adds links like this to `org-mode':
;; - code:orly/orly.el:orly-setup-links/a309064
;; - code:orly/orly.el#54/a309064
;;
;; Advantages over the existing [file:...] link:
;; * Repositores can be moved around without changing the link
;; * More readable links: use functions instead of line numbers
;; * Visit the link on remote if needed
;; * Completion for each part of the link

;;; Code:
(defvar orly-repos-file nil
  "An `org-mode' file name to be processed by `orly-read-repos-from-file'.
Each repo should be a \"file:~/foo/bar\".
All lines that don't match this will be ignored.")

(defun orly-read-repos-from-file (org-fname)
  "Read repos alist from ORG-FNAME."
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

(defun orly-read-repos-from-attachments ()
  "Read Git repos from the current file's attachments."
  (let ((fs (counsel-org-files))
        res)
    (dolist (f fs)
      (when (and (file-directory-p f)
                 (file-exists-p (expand-file-name ".git" f)))
        (push (list (file-name-nondirectory f) (expand-file-name f))
              res)))
    res))

(defun orly-repos ()
  "Return an alist of repository names to repository locations."
  (append
   (orly-read-repos-from-file orly-repos-file)
   (orly-read-repos-from-attachments)))

(defun orly-find-loc (loc)
  "Find location LOC.
LOC is a line number if it starts with #.
LOC is a function name if starts with :."
  (if (string-prefix-p "#" loc)
      (goto-line (string-to-number (substring loc 1)))
    (orly-find-function (substring loc 1))))

(defun orly-open-code-link (code-link)
  "Open CODE-LINK.
CODE-LINK is REPO/FNAME:FUN/REV.
The last 2 parts are optional."
  (if (string-match "\\`\\([^/]+\\)/\\([^:#]+\\)\\([:#].*\\)?\\'" code-link)
      (let* ((repo (match-string 1 code-link))
             (path (match-string 2 code-link))
             (rest (match-string 3 code-link))
             (local-repo (assoc repo (orly-repos)))
             (fname (if local-repo
                        (expand-file-name path (nth 1 local-repo))
                      (error "Could not find repo: %s" repo))))
        (if rest
            (let* ((parts (split-string rest "/"))
                   (loc (nth 0 parts)))
              (cl-case (length parts)
                (1
                 (find-file fname)
                 (orly-find-loc loc))
                (2
                 (let ((rev (nth 1 parts)))
                   (switch-to-buffer (vc-find-revision fname rev))
                   (orly-find-loc loc)))))
          (find-file fname)))
    (error "Failed to parse link: '%s'" code-link)))

(cl-defgeneric orly-find-function (fun)
  "Find FUN in the current file."
  (goto-char (point-min))
  (search-forward fun nil t))

(cl-defmethod orly-find-function (fun &context (major-mode (eql python-mode)))
  "Find FUN in the current file.
Specialized for MAJOR-MODE `python-mode'."
  (goto-char (point-min))
  (search-forward (concat "def " fun))
  (require 'lpy)
  (lpy-back-to-special))

(defun orly--complete-commits (repo-path rev)
  "List completions for commits in REPO-PATH.
REV is passed to `all-completions'."
  (let* ((default-directory repo-path)
         (commits (nreverse
                   (split-string
                    (shell-command-to-string
                     "git log -5 --pretty=format:'%h|%B'")
                    "\n" t)))
         (cl nil))
    (dolist (commit commits)
      (push (split-string commit "|") cl))
    (list (- (point) (length rev))
          (point)
          (all-completions rev cl)
          :annotation-function (lambda (s) (concat " " (cadr (assoc s cl)))))))

(defun orly--complete-tags (fname tag)
  "Complete tags in FNAME.
TAG is passed to `all-completions'."
  (let ((tags
         (with-current-buffer (find-file-noselect fname)
           (mapcar #'car
                   (cl-remove-if-not
                    (lambda (tag) (eq (semantic-tag-class tag) 'function))
                    (counsel-semantic-tags))))))
    (list (- (point) (length tag))
          (point)
          (all-completions tag tags))))

(defun orly--complete-lines (fname line)
  (let ((lines nil)
        (i 1))
    (with-current-buffer (find-file-noselect fname)
      (goto-char (point-min))
      (while (not (eobp))
        (push (format "%d %s" i (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position)))
              lines)
        (cl-incf i)
        (forward-line 1)))
    (setq lines (nreverse lines))
    (list (- (point) (length line))
          (point)
          (cl-remove-if-not (lambda (s) (string-match-p line s)) lines))))

(defun orly-completion-code ()
  "Completion for code: links in `org-mode'."
  (when (looking-back "code:\\([-:A-Za-z0-9./_#]*\\)" (line-beginning-position))
    (let ((link (match-string-no-properties 1))
          (mb (match-beginning 1))
          (repos (orly-repos))
          rev-part tag-part)
      (if (string-match "\\`\\([^/]+\\)/\\([^:#]*\\)\\([:#][^/]*\\(/.*\\)?\\)?\\'" link)
          (let* ((repo (match-string 1 link))
                 (repo-path (expand-file-name (nth 1 (assoc repo repos)))))
            (cond ((setq rev-part (match-string 4 link))
                   (orly--complete-commits repo-path (substring rev-part 1)))
                  ((setq tag-part (match-string 3 link))
                   (let ((fname (expand-file-name (match-string 2 link) repo-path)))
                     (if (string-prefix-p ":" tag-part)
                         (orly--complete-tags
                          fname
                          (substring tag-part 1))
                       (orly--complete-lines
                        fname
                        (substring tag-part 1)))))
                  (t
                   (let* ((path (match-string 2 link))
                          (full-path (if (string= path "")
                                         (file-name-as-directory repo-path)
                                       (expand-file-name path repo-path)))
                          (part (file-name-nondirectory full-path))
                          (default-directory (file-name-directory full-path))
                          (beg (- (+ mb (length repo) (length path) 1) (length part))))
                     (list beg (+ beg (length part))
                           (all-completions part #'read-file-name-internal
                                            (lambda (s)
                                              (not (or (string-match-p "\\`\\(\\.\\|__\\)" s)
                                                       (string-match-p "~\\'" s))))))))))
        (list mb (+ mb (length link))
              (mapcar (lambda (s) (concat s "/"))
                      (all-completions link repos)))))))

(org-link-set-parameters "code" :follow #'orly-open-code-link)
(cl-pushnew 'orly-completion-code orly-completion-functions)

(provide 'orly-code)

;;; orly-code.el ends here
