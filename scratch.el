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
;; TODO: persistent counter
;; TODO: rules for ignoring in recentf
;; TODO: if consult, then preview file in scratch-open?
;; TODO: better filenames

;;; Code:

(defgroup scratch nil
  "Welcome buffer."
  :group 'convenience
  :prefix "scratch-")

(defcustom scratch-directory
  (expand-file-name "scratch/" user-emacs-directory)
  "Folder in which to store scratch files.")

;; TODO persist this in file
(defvar scratch--count 0)

(defun scratch--buffer-name (file-extension)
  (format "scratch-%d.%s" scratch--count file-extension))

(defun scratch--expand-filename (file-name)
  (let ((default-directory scratch-directory))
    (expand-file-name (format "%d_%s"
                              (time-convert nil 'integer)
                              file-name))))

(defun scratch--buffer-mode (buffer-name)
  (cdr (assoc buffer-name auto-mode-alist #'string-match)))

(defun scratch--count (operation)
  (setq scratch--count (funcall operation scratch--count 1)))

(defun scratch--save (&optional scratch-buffer)
  (let ((buffer (or scratch-buffer (current-buffer))))
    (with-current-buffer buffer
      (when scratch-mode
        (unless (file-exists-p scratch-directory)
          (make-directory scratch-directory t))
        (write-region (point-min)
                      (point-max)
                      (scratch--expand-filename (buffer-name)))))))

(defun scratch--save-all ()
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'scratch-mode buffer)
      (scratch--save buffer))))

;;;###autoload
(define-minor-mode scratch-mode
  "Minor mode for scratch buffers."
  :global nil
  :group 'scratch
  (if scratch-mode
      (progn
        (add-hook 'kill-buffer-hook #'scratch--save nil t))
    (remove-hook 'kill-buffer-hook #'scratch--save t)))

;;;###autoload
(define-minor-mode scratch-global-mode
  "Global minor mode for managing scratch buffers."
  :global t
  :group 'scratch
  (if scratch-global-mode
      (progn
        (add-hook 'kill-emacs-hook #'scratch--save-all))
    (remove-hook 'kill-emacs-hook #'scratch--save-all)))

;;;###autoload
(defun scratch-named (buffer-name)
  "TODO"
  (interactive "MScratch name: ")
  (with-current-buffer (get-buffer-create buffer-name)
    (funcall (scratch--buffer-mode buffer-name))
    (scratch-mode +1)
    (scratch--count '+))
  (switch-to-buffer buffer-name))

;;;###autoload
(defun scratch (file-extension)
  "TODO."
  (interactive "MAuto mode extension: ")
  (scratch-named (scratch--buffer-name file-extension)))

(defcustom scratch-find-file-function
  'find-file
  "asdasd")

;;;###autoload
(defun scratch-open ()
  (interactive)
  (let ((default-directory scratch-directory))
    (call-interactively scratch-find-file-function)))

(provide 'scratch)
;;; scratch.el ends here
