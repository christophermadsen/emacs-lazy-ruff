;;; lazy-ruff.el --- Integration with the Ruff Python linter/formatter -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Christopher Buch Madsen

;; Author: Christopher Buch Madsen
;; Version: 0.2
;; Package-Requires: ((emacs "24.3") (org "9.1"))
;; Keywords: languages, tools
;; URL: http://github.com/yourusername/emacs-lazy-ruff

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides Emacs commands to format and lint Python code using
;; the external 'ruff' tool.  It offers functions to format the entire buffer,
;; specific regions, or Org mode source blocks.
;; 
;; Prerequisites:
;; - The 'ruff' command-line tool must be installed and available in your
;;   system's PATH.
;;
;; Installation of 'ruff':
;; - Please refer to the 'ruff' documentation at:
;;   https://docs.astral.sh/ruff/installation/
;;
;; Configuring lazy-ruff:
;; The global variables defined in the part before the ruff-lint-format-block
;; function can be freely changed with setq in your init.el (or other
;; personalization files) for configuring lazy-ruff.

;;; Code:


(require 'org-element)
(require 'org)

(defvar python-mode-hook) ;; Quiet the byte-compiler warning

(defvar lazy-ruff-check-command "ruff check --fix -s"
  "Defines the ruff check call for all methods.")

(defvar lazy-ruff-format-command "ruff format -s"
  "Defines the ruff format call for all methods.")

(defvar lazy-ruff-only-format-block nil
  "When non-nil (e.g. t), only format the code in a block without linting fixes.")

(defvar lazy-ruff-only-format-buffer nil
  "When non-nil (e.g. t), only format the code in a buffer without linting fixes.")

(defvar lazy-ruff-only-format-region nil
  "When non-nil (e.g. t), only format the code in a region without linting fixes.")

;;;###autoload
(defun lazy-ruff-lint-format-block ()
  "Format Python `org-babel` blocks in Org mode using `ruff`.
Ensures cursor position is maintained.  Requires `ruff` in system's PATH."

  (interactive)
  (unless (org-in-src-block-p) (error "Not inside a source block"))

  (let ((initial-line (line-number-at-pos))
        (initial-column (current-column)))
    (let* ((element (org-element-context))
           (lang (org-element-property :language element))
           (code (org-element-property :value element))
           (temp-file (make-temp-file "emacs-org-ruff" nil ".py"))
           formatted-code
           (content-start (save-excursion
                            (goto-char (org-element-property :begin element))
                            (search-forward (concat "#+BEGIN_SRC " lang))
                            (line-end-position)))
           (content-end (save-excursion
                          (goto-char (org-element-property :end element))
                          (search-backward "#+END_SRC")
                          (line-beginning-position))))
      (if (not (string= lang "python"))
          (message "The source block is not Python")
        (with-temp-file temp-file (insert code))
        (if lazy-ruff-only-format-block
            (shell-command-to-string (format "%s %s" lazy-ruff-format-command temp-file))
          (shell-command-to-string (format "%s %s" lazy-ruff-check-command temp-file))
          (shell-command-to-string (format "%s %s" lazy-ruff-format-command temp-file)))
        (setq formatted-code (with-temp-buffer
                               (insert-file-contents temp-file)
                               (buffer-string)))
        (delete-region (1+ content-start) content-end)
        (goto-char (1+ content-start))
        (insert formatted-code "\n"))
      (delete-file temp-file))
    (goto-char (point-min))
    (forward-line (1- initial-line))
    (move-to-column initial-column)))

;;;###autoload
(defun lazy-ruff-lint-format-buffer ()
  "Format the current Python buffer using `ruff` before saving."
  (interactive)
  (when (eq major-mode 'python-mode)
    (let ((temp-file (make-temp-file "ruff-tmp" nil ".py")))
      ;; Write buffer to temporary file, format it, and replace buffer contents.
      (write-region nil nil temp-file)
      (if lazy-ruff-only-format-buffer
          (shell-command-to-string (format "%s %s" lazy-ruff-format-command temp-file))
        (shell-command-to-string (format "%s %s" lazy-ruff-check-command temp-file))
        (shell-command-to-string (format "%s %s" lazy-ruff-format-command temp-file)))
      (erase-buffer)
      (insert-file-contents temp-file)
      ;; Clean up temporary file.
      (delete-file temp-file))))

;;;###autoload
(defun lazy-ruff-lint-format-region ()
  "Format the currently selected region using `ruff`.  Use at your own discretion."
  (interactive)
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (temp-file (make-temp-file "ruff-region-tmp" nil ".py"))
             (temp-buffer (generate-new-buffer " *temp-ruff-output*")))
        ;; Write selected region to temporary file, format it.
        (write-region start end temp-file nil 'silent)
        (if lazy-ruff-only-format-region
            (shell-command-to-string (format "%s %s" lazy-ruff-format-command temp-file))
          (shell-command-to-string (format "%s %s" lazy-ruff-check-command temp-file))
          (shell-command-to-string (format "%s %s" lazy-ruff-format-command temp-file)))
        ;; Replace region with formatted content.
        (with-current-buffer temp-buffer
          (insert-file-contents temp-file))
        (delete-region start end)
        (insert-buffer-substring temp-buffer)
        
        ;; Cleanup actions.
        (delete-file temp-file)
        (kill-buffer temp-buffer))
    (message "No region selected.")))

;;;###autoload
(defun lazy-ruff-lint-format-dwim ()
  "Dispatch to the correct ruff format function based on the context."
  (interactive)
  ;; First, check if a region is selected
  (if (use-region-p)
      (lazy-ruff-lint-format-region)
    ;; Next, check if inside an org-babel code block
    (if (org-in-src-block-p)
        (lazy-ruff-lint-format-block)
      ;; Lastly, check if the current buffer is a Python mode buffer
      (if (eq major-mode 'python-mode)
          (lazy-ruff-lint-format-buffer)
        (message "Not in a Python buffer or org-babel block, and no region is selected.")))))

;;;###autoload
(define-minor-mode lazy-ruff-mode
  "Toggle automatic formatting with Ruff in a Python buffer."
  :lighter " Lazy-Ruff"
  :global nil
  (if lazy-ruff-mode
      (add-hook 'before-save-hook #'lazy-ruff-lint-format-buffer nil t)
    (remove-hook 'before-save-hook #'lazy-ruff-lint-format-buffer t)))

;;;###autoload
(defun lazy-ruff-mode-global-toggle (&optional enable)
  "Toggle or explicitly set `lazy-ruff-mode` globally for Python buffers.
With no argument, toggles the mode.  With a non-nil argument ENABLE, turns the
mode on, and with nil, turns it off."
  (interactive "P")
  (let ((target-state (if (called-interactively-p 'any)
                          (not (memq 'lazy-ruff-mode python-mode-hook))
                        enable)))
    ;; Ensure hooks and mode are aligned with target state
    (if target-state
        (add-hook 'python-mode-hook #'lazy-ruff-mode)
      (remove-hook 'python-mode-hook #'lazy-ruff-mode))
    ;; Apply the mode state to all current Python buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'python-mode)
          (lazy-ruff-mode (if target-state 1 -1)))))))

(provide 'lazy-ruff)
;;; lazy-ruff.el ends here
