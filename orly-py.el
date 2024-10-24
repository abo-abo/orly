;;; orly-py.el --- open e.g. py:typing.Any links     -*- lexical-binding: t; -*-
;;; Code:

(require 'le-python)
(require 'lpy)


(defvar orly-get-python-process-function #'lispy--python-proc
  "Get us the current or a fresh Python process.

Customizing it per org document results in Python links resolving
to a different venv/environment that's specific to the document.")


(defun orly-open-py-link (link)
  (when (string-match "\\`\\([^/]+\\)\\(?:/\\(.*\\)\\)?\\'" link)
    (let* ((code (match-string 1 link))
           (text (match-string 2 link))
           (proc (funcall orly-get-python-process-function))
           (buf (process-buffer proc))
           (definition
            (with-current-buffer buf
              (python-shell-send-string-no-output
               (format
                "lp.goto_link_definition('%s')"
                code)
               proc))))
      (unless (string= definition "")
        (xref-push-marker-stack)
        (cl-destructuring-bind (fname line column) (read definition)
          (lispy--goto-symbol-python fname line))
        (when text
          (let ((bnd (lispy--bounds-dwim)))
            (when (or (re-search-forward (concat " *\\(def \\)?" text) (cdr bnd) t)
                      (re-search-forward text nil t))
              (lpy-back-to-special))))
        t))))

(org-link-set-parameters "py" :follow #'orly-open-py-link)

(defun orly-py-store-link ()
  (interactive)
  (let* ((func (python-info-current-defun))
         (import (lispy--eval-python-plain
                  (format "lp.get_import_name('%s')" (buffer-file-name))))
         (url (concat "py:" (replace-regexp-in-string
                              "\\.__init__"
                              ""
                              (substring import 1 -1)) "." func)))
    (kill-new (format "[[%s][%s]]" url func))
    (message url)))


(provide 'orly-py)
;;; orly-py.el ends here
