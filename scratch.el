;;; scratch.el --- Minor mode for managing scratch buffers -*- lexical-binding: t; -*-

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

;; Minor mode for managing scratch buffers.
;; A "scratch" buffer is some piece of text or code that we don't
;; want to explicitly save to a file, but we might want to recover
;; across sessions.

;; TODO: docs
;; TODO: rules for ignoring in recentf
;; TODO: if consult, then preview file in scratch-open?
;; TODO: better filenames

;;; Code:

(defgroup scratch nil
  "Scratch management."
  :group 'convenience
  :prefix "scratch-")

(defcustom scratch-directory
  (expand-file-name "scratch/" user-emacs-directory)
  "Folder in which to store scratch files."
  :group 'scratch
  :type 'string)

(defcustom scratch-find-file-function
  'find-file
  "Function used for finding scratch files."
  :group 'scratch
  :type 'function)

(defcustom scratch-persist-excluded-modes nil
  "Major modes to exclude from persisting when `scratch-persist-mode' is active"
  :group 'scratch
  :type 'list)

(defcustom scratch-unique-buffer-name-fn
  'scratch-unique-buffer-name
  "TODO"
  :group 'scratch
  :type 'function)

(defun scratch--expand-filename (buffer-name)
  "File name of scratch buffer named BUFFER-NAME."
  (expand-file-name buffer-name scratch-directory))

(defun scratch--buffer-mode (buffer-name)
  "Major mode of buffer named BUFFER-NAME.

Major mode is determined by matching with `auto-mode-alist'."
  (cdr (assoc buffer-name auto-mode-alist #'string-match)))

(defun scratch--save (&optional scratch-buffer)
  "Save a SCRATCH-BUFFER as file.

SCRATCH-BUFFER defaults to the current buffer."
  (let ((buffer (or scratch-buffer (current-buffer))))
    (with-current-buffer buffer
      (when (and scratch-mode
                 (not (memq major-mode scratch-persist-excluded-modes)))
        (unless (file-exists-p scratch-directory)
          (make-directory scratch-directory t))
        (write-region (point-min)
                      (point-max)
                      (scratch--expand-filename (buffer-name)))))))

(defun scratch--save-all ()
  "Save all scratch buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'scratch-mode buffer)
      (scratch--save buffer))))

(defun scratch-unique-buffer-name (buffer-name)
  "Unique name for the scratch buffer based on BUFFER-NAME"
  (let ((base (file-name-sans-extension buffer-name))
        (ext (file-name-extension buffer-name)))
    (concat base
            "_"
            (format-time-string "%Y-%m-%dT%H-%M-%S")
            (when ext
              (concat "." ext)))))

;;;###autoload
(define-minor-mode scratch-mode
  "Minor mode for scratch buffers."
  :global nil
  :group 'scratch)

;;;###autoload
(define-minor-mode scratch-persist-mode
  "Global minor mode for persisting scratch buffers."
  :global t
  :group 'scratch
  (if scratch-persist-mode
      (progn
        (add-hook 'kill-buffer-hook #'scratch--save)
        (add-hook 'kill-emacs-hook #'scratch--save-all))
    (remove-hook 'kill-buffer-hook #'scratch--save)
    (remove-hook 'kill-emacs-hook #'scratch--save-all)))

;;;###autoload
(defun scratch-named (buffer-name)
  "Create new scratch buffer named BUFFER-NAME."
  (interactive "MScratch name: ")
  (with-current-buffer (get-buffer-create
                        (funcall scratch-unique-buffer-name-fn buffer-name))
    (funcall (scratch--buffer-mode buffer-name))
    (scratch-mode +1))
  (switch-to-buffer buffer-name))

;;;###autoload
(defun scratch (mode &optional buffer-name)
  (interactive
   (list
    (intern
     (completing-read
      "Major mode: "
      (delete-dups (mapcar 'cdr auto-mode-alist))))
    (when current-prefix-arg
      (read-string "Scratch buffer name: "))))
  ;; refactor this ugly buffer name handling
  (let ((unique-buffer-name
         (funcall scratch-unique-buffer-name-fn
                  (or buffer-name "scratch"))))
    (with-current-buffer (get-buffer-create unique-buffer-name)
      (funcall mode)
      (add-file-local-variable-prop-line 'mode mode)
      (goto-char (point-max))
      (scratch-mode +1))
    (switch-to-buffer unique-buffer-name)))

;;;###autoload
(defun scratch-open ()
  "Open a scratch buffer from a previeous session."
  (interactive)
  (let ((default-directory scratch-directory))
    (call-interactively scratch-find-file-function)))

(provide 'scratch)
;;; scratch.el ends here
