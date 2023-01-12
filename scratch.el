;;; scratch.el --- Manage scratch buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com>
;; Maintainer: Tommaso Rossi <tommaso.rossi1@protonmail.com>
;; Created: 2022
;; Version: 0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/tomrss/scratch.el

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

;; Utilities for managing scratch buffers.  A "scratch" buffer is a
;; temporary buffer used for holding copy-paste content, for writing
;; quick notes, for writing and evaluating some piece of code, or
;; other.  This package helps dealing whith such common tasks and also
;; (optionally) to persist scratch buffers for later sessions.

;; TODO: do not save empty scratches
;; TODO: implement open last function

;;; Code:

(defgroup scratch nil
  "Manage scratch buffers."
  :group 'convenience
  :prefix "scratch-")

(defcustom scratch-directory
  (locate-user-emacs-file "scratch/")
  "Folder in which to store scratch files."
  :group 'scratch
  :type 'string)

(defcustom scratch-find-file-fn
  'find-file
  "Function used for finding scratch files."
  :group 'scratch
  :type 'function)

(defcustom scratch-major-mode-list-fn
  'scratch-major-mode-list
  "Function that lists major modes available for scratch buffers."
  :group 'scratch
  :type 'function)

(defcustom scratch-history-limit 100
  "Limit number of scratch buffers to be saved."
  :group 'scratch
  :type 'integer)

(defcustom scratch-persist-excluded-modes nil
  "Major modes to exclude from saving."
  :group 'scratch
  :type 'list)

(defcustom scratch-sorting-predicate
  'scratch-sort-by-progressive
  "Predicate using for sorting saved scratches.

The predicate accepts two file and attribute cons (as returned by
`directory-files-and-attributes')"
  :group 'scratch
  :type 'function)

(defcustom scratch-search-fn
  'scratch-grep
  "Grep-like command for searching in scratch files."
  :group 'scratch
  :type 'function)

;; TODO probably too dangerous to make customizable
(defconst scratch--file-name-regexp "^\\(.*\\)\\-\\-\\([0-9]+\\)\\(\\..*\\)?\\'"
  "Regex for matching scratch files.")

;;;###autoload
(define-minor-mode scratch-mode
  "Minor mode for scratch buffers."
  :global nil
  :group 'scratch)

(defun scratch--parse-progressive (file-name)
  "Parse scratch progressive from FILE-NAME."
  (if (string-match scratch--file-name-regexp file-name)
      (string-to-number (match-string 2 file-name))
    -1))

(defun scratch-sort-by-modification-time (x y)
  "Sorting predicate based on file modification time for files/attrs X and Y."
  (time-less-p (file-attribute-modification-time (cdr y))
               (file-attribute-modification-time (cdr x))))

(defun scratch-sort-by-progressive (x y)
  "Sorting predicate based on scratch progressive for files/attrs X and Y."
  (> (scratch--parse-progressive (car x))
     (scratch--parse-progressive (car y))))

(defun scratch--list-saved ()
  "List saved scratch files sorted with `scratch-sorting-predicate'."
  (sort (directory-files-and-attributes scratch-directory
                                        'full
                                        scratch--file-name-regexp
                                        'nosort)
        scratch-sorting-predicate))

(defun scratch--check-delete-oldest ()
  "Check if some old scratch files have to be deleted.

Deleted files are the oldest (in the sense of `scratch-sorting-predicate') ones
who exceed `scratch-history-limit'."
  (let* ((scratches (scratch--list-saved))
         (total (length scratches))
         (exceeding (- total scratch-history-limit)))
    (when (> exceeding 0)
      (dolist (to-delete (last scratches exceeding))
        (delete-file (car to-delete)))
      (message "Deleted %d old scratch files." exceeding))))

(defun scratch-save (&optional scratch-buffer)
  "Save a SCRATCH-BUFFER as file.

SCRATCH-BUFFER defaults to the current buffer."
  (interactive)
  (let ((buffer (or scratch-buffer (current-buffer))))
    (with-current-buffer buffer
      (when (and scratch-mode
                 (not (memq major-mode scratch-persist-excluded-modes)))
        (unless (file-exists-p scratch-directory)
          (make-directory scratch-directory t))
        (write-region (point-min)
                      (point-max)
                      (expand-file-name (buffer-name) scratch-directory))
        (scratch--check-delete-oldest)))))

(defun scratch-save-all ()
  "Save all scratch buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'scratch-mode buffer)
      (scratch-save buffer))))

(defun scratch--append-to-file-name (file-name &rest sequences)
  "Append SEQUENCES to FILE-NAME respecting FILE-NAME extension."
  (let ((base (file-name-sans-extension file-name))
        (extension (file-name-extension file-name)))
    (concat base
            (apply #'concat sequences)
            (when extension
              (concat "." extension)))))

(defun scratch--max-file-progressive ()
  "Get max progressive from saved scratches."
  (if-let ((progressives
            (mapcar
             #'scratch--parse-progressive
             (directory-files scratch-directory nil scratch--file-name-regexp))))
      (apply #'max progressives)
    -1))

(defun scratch--max-buffer-progressive ()
  "Get max progressive from current scratch buffers."
  (if-let ((progressives
            (remove nil
                    (mapcar
                     (lambda (buffer)
                       (when (buffer-local-value 'scratch-mode buffer)
                         (scratch--parse-progressive (buffer-name buffer))))
                     (buffer-list)))))
      (apply #'max progressives)
    -1))

(defun scratch--unique-name (buffer-name)
  "Get a unique name for new scratch buffer based on BUFFER-NAME.

A progressive is computed and added (respecting file extension) to BUFFER-NAME."
  (scratch--append-to-file-name
   buffer-name
   ;; TODO: this separator is too closely related to used regex
   "--"
   (number-to-string
    (+ 1 (max (scratch--max-file-progressive)
              (scratch--max-buffer-progressive))))))

(defun scratch-major-mode-list ()
  "Return list of major modes defined in `auto-mode-alist'."
  (delete-dups (mapcar 'cdr auto-mode-alist)))

(defun scratch--buffer-mode (buffer-name)
  "Major mode of buffer named BUFFER-NAME.

Major mode is determined by matching with `auto-mode-alist'."
  (cdr (assoc buffer-name auto-mode-alist #'string-match)))

;;;###autoload
(defun scratch-titled (file-name)
  "Create new scratch buffer with unique name based on FILE-NAME.

The major mode will be determined by matching `auto-mode-alist' with
FILE-NAME."
  (interactive "MScratch filename: ")
  (let ((scratch-buffer-name (scratch--unique-name file-name)))
    (with-current-buffer (get-buffer-create scratch-buffer-name)
      (funcall (scratch--buffer-mode file-name))
      (goto-char (point-min))
      (insert "Title: ")
      (insert file-name)
      (when comment-start
        (comment-line 1))
      (goto-char (point-max))
      (newline)
      (insert "Time-stamp: <>")
      (when comment-start
        (comment-line 1))
      (goto-char (point-max))
      (newline)
      (newline)
      (time-stamp)
      (scratch-mode +1))
    (switch-to-buffer scratch-buffer-name)))

;;;###autoload
(defun scratch-new (mode &optional buffer-name)
  "Create new scratch buffer with major mode MODE.

If BUFFER-NAME is specified, the scratch name will have a unique name based on
it.  If not, a unique name will be created.  When called interactively,
BUFFER-NAME can be specified via universal argument."
  (interactive
   (list
    (intern (completing-read "Major mode: "
                             (funcall scratch-major-mode-list-fn)))
    (when current-prefix-arg
      (read-string "Scratch buffer name: "))))
  (let* ((string-mode-name (string-replace "-mode" "" (symbol-name mode)))
         (scratch-buffer-name
          (scratch--unique-name (or buffer-name
                                    (concat string-mode-name "-" "scratch")))))
    (with-current-buffer (get-buffer-create scratch-buffer-name)
      (funcall mode)
      (unless (eq 'fundamental-mode mode)
        ;; cannot put directly `mode' in file local: in that case, when opening
        ;; file emacs will try to load with an extra -mode (e.g. c++-mode-mode)
        (add-file-local-variable-prop-line 'mode (intern string-mode-name))
        (goto-char (point-max))
        (newline))
      (scratch-mode +1))
    (switch-to-buffer scratch-buffer-name)))

;;;###autoload
(defun scratch-open ()
  "Open a scratch buffer from a previeous session."
  (interactive)
  (let ((default-directory scratch-directory))
    (call-interactively scratch-find-file-fn)))

(defun scratch-grep (regexp)
  "Grep REGEXP in scratch files."
  (interactive "MRegexp: ")
  (let ((default-directory scratch-directory))
    (lgrep regexp "*")))

;;;###autoload
(defun scratch-search ()
  "Grep-like search in scratch files."
  (interactive)
  (let ((default-directory scratch-directory))
    (call-interactively scratch-search-fn)))

;;;###autoload
(define-minor-mode scratch-persist-mode
  "Global minor mode for persisting scratch buffers.

When this global mode is active, scratch buffers will be automatically saved in
`scratch-directory' when they are killed on when Emacs is closed."
  :global t
  :group 'scratch
  (if scratch-persist-mode
      (progn
        (unless (file-exists-p scratch-directory)
          (make-directory scratch-directory t))
        (add-hook 'kill-buffer-hook #'scratch-save)
        (add-hook 'kill-emacs-hook #'scratch-save-all))
    (remove-hook 'kill-buffer-hook #'scratch-save)
    (remove-hook 'kill-emacs-hook #'scratch-save-all)))

;;;###autoload (autoload 'scratch-key-map "scratch" nil nil 'keymap)
(eval-and-compile
  (define-prefix-command 'scratch-key-map))

(define-key scratch-key-map (kbd "n") #'scratch-new)
(define-key scratch-key-map (kbd "t") #'scratch-titled)
(define-key scratch-key-map (kbd "o") #'scratch-open)
(define-key scratch-key-map (kbd "r") #'scratch-search)

(provide 'scratch)
;;; scratch.el ends here
