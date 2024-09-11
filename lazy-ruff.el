;;; lazy-ruff.el --- Integration with the Ruff Python linter/formatter -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Christopher Buch Madsen

;; Author: Christopher Buch Madsen
;; Version: 0.2.4
;; Package-Requires: ((emacs "24.3") (org "9.1"))
;; Keywords: languages, tools
;; URL: http://github.com/christophermadsen/emacs-lazy-ruff

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
;;   system's PATH. Please refer to the 'ruff' documentation at:
;;   https://docs.astral.sh/ruff/installation/
;;
;; lazy-ruff quickstart with use-package:
;; (use-package lazy-ruff
;;   :ensure t
;;   :bind (("C-c f" . lazy-ruff-lint-format-dwim)) ;; keybinding
;;   :config
;;   (lazy-ruff-global-mode t)) ;; Enable the lazy-ruff minor mode globally
;;
;; For further information on how to use lazy-ruff, please refer to the README at:
;; https://github.com/christophermadsen/emacs-lazy-ruff

;;; Code:


(require 'org-element)
(require 'org)
(require 'python)

(defvar lazy-ruff-check-command "ruff check --fix -s"
  "Defines the ruff check call for all methods.")

(defvar lazy-ruff-format-command "ruff format -s"
  "Defines the ruff format call for all methods.")

(defvar lazy-ruff-only-format-block nil
  "When non-nil (e.g. t), only format the code in a block without linting fixes.")

(defvar lazy-ruff-only-check-block nil
  "When non-nil (e.g. t), only lint the code in a block without formatting fixes.")

(defvar lazy-ruff-only-format-buffer nil
  "When non-nil (e.g. t), only format the code in a buffer without linting fixes.")

(defvar lazy-ruff-only-check-buffer nil
  "When non-nil (e.g. t), only lint the code in a buffer without formatting fixes.")

(defvar lazy-ruff-only-format-region nil
  "When non-nil (e.g. t), only format the code in a region without linting fixes.")

(defvar lazy-ruff-only-check-region nil
  "When non-nil (e.g. t), only lint the code in a region without formatting fixes.")


(defun lazy-ruff-run-commands (temp-file only-format only-check)
  "Run the appropriate ruff commands on TEMP-FILE.
If ONLY-FORMAT is true, only format the file.
If ONLY-CHECK is true, only check the file.
Otherwise, run both check and format commands."
  (cond
   (only-format
    (shell-command-to-string (format "%s %s" lazy-ruff-format-command temp-file)))

   (only-check
    (shell-command-to-string (format "%s %s" lazy-ruff-check-command temp-file)))

   (t
    (progn
      (shell-command-to-string (format "%s %s" lazy-ruff-check-command temp-file))
      (shell-command-to-string (format "%s %s" lazy-ruff-format-command temp-file))))))

;;;###autoload
(defun lazy-ruff-lint-format-block ()
  "Format Python `org-babel` blocks in Org mode using `ruff`.
Ensures cursor position is maintained.  Requires `ruff` in system's PATH."
  (interactive)
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
	(lazy-ruff-run-commands temp-file (eq lazy-ruff-only-format-block t) (eq lazy-ruff-only-check-block t))
        (setq formatted-code (with-temp-buffer
                               (insert-file-contents temp-file)
                               (buffer-string)))
        (delete-region (1+ content-start) content-end)
        (goto-char (1+ content-start))
        (insert formatted-code))
      (delete-file temp-file))
    (goto-char (point-min))
    (forward-line (1- initial-line))
    (move-to-column initial-column)))

;;;###autoload
(defun lazy-ruff-lint-format-buffer ()
  "Format the current Python buffer using `ruff` before saving."
  (interactive)
  (unless (derived-mode-p 'python-mode 'python-base-mode)
    (user-error "Only python buffers can be linted with ruff"))
  (let ((temp-file (make-temp-file "ruff-tmp" nil ".py")))
    ;; Write buffer to temporary file, format it, and replace buffer contents.
    (write-region nil nil temp-file)
	(lazy-ruff-run-commands temp-file (eq lazy-ruff-only-format-buffer t) (eq lazy-ruff-only-check-buffer t))
    (erase-buffer)
    (insert-file-contents temp-file)
    ;; Clean up temporary file.
    (delete-file temp-file)))

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
	(lazy-ruff-run-commands temp-file (eq lazy-ruff-only-format-region t) (eq lazy-ruff-only-check-region t))
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
  (cond
   ;; First, check if a region is selected
   ((use-region-p)
    (lazy-ruff-lint-format-region))
   ;; Next, check if inside an org-babel code block
   ((org-in-src-block-p)
    (lazy-ruff-lint-format-block))
   ;; Lastly, check if the current buffer is a Python mode buffer
   ((derived-mode-p 'python-mode 'python-base-mode)
    (lazy-ruff-lint-format-buffer))
   (t
    (message "Not in a Python buffer or org-babel block, and no region is selected."))))

;;;###autoload
(define-minor-mode lazy-ruff-mode
  "Toggle automatic formatting with Ruff in a Python buffer."
  :lighter " Lazy-Ruff"
  :global nil
  (if lazy-ruff-mode
      (add-hook 'before-save-hook #'lazy-ruff-lint-format-buffer nil t)
    (remove-hook 'before-save-hook #'lazy-ruff-lint-format-buffer t)))

;;;###autoload
(define-globalized-minor-mode lazy-ruff-global-mode lazy-ruff-mode
  (lambda () (when (derived-mode-p 'python-mode 'python-base-mode)
               (lazy-ruff-mode 1))))

;;;###autoload
(define-obsolete-function-alias 'lazy-ruff-mode-global-toggle 'lazy-ruff-global-mode "0.2.2")


(provide 'lazy-ruff)
;;; lazy-ruff.el ends here
