;;; ivy-file-preview.el --- Preview the current ivy file selection  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-09-07 23:42:35

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Preview the current ivy file selection.
;; Keyword: file ivy swiper preview select selection
;; Version: 0.1.3
;; Package-Requires: ((emacs "25.1") (ivy "0.8.0") (f "0.20.0"))
;; URL: https://github.com/jcs090218/ivy-file-preview

;; This file is NOT part of GNU Emacs.

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
;;
;; Preview the current ivy file selection.
;;

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'ivy)

(defgroup ivy-file-preview nil
  "Preview the current ivy file selection."
  :prefix "ivy-file-preview-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/ivy-file-preview"))

(defcustom ivy-file-preview-preview-only t
  "Preview the file instead of actually opens the file."
  :type 'boolean
  :group 'ivy-file-preview)

(defcustom ivy-file-preview-details t
  "Preview file only when there are more details in the selection."
  :type 'boolean
  :group 'ivy-file-preview)

(defvar ivy-file-preview--preview-files '()
  "Files that are previewing, and will be closed after action is done.")

(defvar ivy-file-preview--selected-file ""
  "Record down the current selected file.")

(defvar ivy-file-preview--window-status '()
  "Record windows status for when canceling command.")

;;; Util

(defun ivy-file-preview--project-path ()
  "Get current project path."
  (cdr (project-current)))

(defun ivy-file-preview--goto-line (ln)
  "Goto LN line number."
  (goto-char (point-min))
  (forward-line (1- ln)))

;;; Core

(defun ivy-file-preview--open-file (fn pos)
  "Open the file path (FN).
POS can either be an integer or cons cell represent line number and columns."
  (setq ivy-file-preview--selected-file fn)
  (find-file fn)
  (cond ((consp pos)
         (ivy-file-preview--goto-line (car pos))
         (move-to-column (cdr pos)))
        ((integerp pos) (goto-char (1+ pos)))
        (t (error "Undefined pos details: %s" pos))))

(defun ivy-file-preview--do-preview (project-dir fn pos)
  "Do file preview execution.
FN is the file path.  PROJECT-DIR is the path of the project root directory.
POS can either be an integer or cons cell represent line number and columns."
  (save-selected-window
    (with-selected-window minibuffer-scroll-window
      (when project-dir (setq fn (f-join project-dir fn)))
      (when (and ivy-file-preview-preview-only (not (find-buffer-visiting fn)))
        (push fn ivy-file-preview--preview-files))
      (ivy-file-preview--open-file fn pos))))

(defun ivy-file-preview--after-select (&rest _)
  "Execution after selection."
  (let* ((project-dir (ivy-file-preview--project-path))
         (cands (or ivy--old-cands ivy--all-candidates '()))
         (current-selection (or (nth ivy--index cands) ""))
         (sel-lst (split-string current-selection ":"))
         fn ln cl)
    (when (< 2 (length sel-lst))
      (setq fn (nth 0 sel-lst) ln (nth 1 sel-lst) cl (nth 2 sel-lst)))
    (when (and ivy-file-preview-details ln)
      (setq ln (string-to-number ln)
            cl (ignore-errors (cl-parse-integer cl)))
      (ivy-file-preview--do-preview project-dir fn (if cl (cons ln cl) ln)))))

(defun ivy-file-preview--cancel-revert ()
  "Revert frame status if user cancel the commands."
  (unless ivy-exit
    (switch-to-buffer (pop ivy-file-preview--window-status))
    (set-window-point minibuffer-scroll-window (pop ivy-file-preview--window-status))))

(defun ivy-file-preview--enter ()
  "Execution after minibuffer setup."
  (setq ivy-file-preview--window-status '())
  (with-selected-window minibuffer-scroll-window
    (push (window-point) ivy-file-preview--window-status)
    (push (buffer-name) ivy-file-preview--window-status)))

(defun ivy-file-preview--exit ()
  "Execution before minibuffer exits."
  (delete-dups ivy-file-preview--preview-files)
  (dolist (fn ivy-file-preview--preview-files)
    (unless (string= ivy-file-preview--selected-file fn)
      (kill-buffer (f-filename fn))))
  (ivy-file-preview--cancel-revert)  ; If already empty, revert it.
  (setq ivy-file-preview--selected-file "")
  (setq ivy-file-preview--preview-files '()))

;;; Entry

(defun ivy-file-preview--enable ()
  "Enable `ivy-file-preview'."
  (add-hook 'minibuffer-setup-hook #'ivy-file-preview--enter)
  (add-hook 'minibuffer-exit-hook #'ivy-file-preview--exit)
  (advice-add 'ivy--exhibit :after #'ivy-file-preview--after-select))

(defun ivy-file-preview--disable ()
  "Disable `ivy-file-preview'."
  (remove-hook 'minibuffer-setup-hook #'ivy-file-preview--enter)
  (remove-hook 'minibuffer-exit-hook #'ivy-file-preview--exit)
  (advice-remove 'ivy--exhibit #'ivy-file-preview--after-select))

;;;###autoload
(define-minor-mode ivy-file-preview-mode
  "Minor mode 'ivy-file-preview-mode'."
  :global t
  :require 'ivy-file-preview
  :group 'ivy-file-preview
  (if ivy-file-preview-mode (ivy-file-preview--enable) (ivy-file-preview--disable)))

(provide 'ivy-file-preview)
;;; ivy-file-preview.el ends here
