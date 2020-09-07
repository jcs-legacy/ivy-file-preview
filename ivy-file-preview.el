;;; ivy-file-preview.el --- Preview the current ivy file selection  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-09-07 23:42:35

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Preview the current ivy file selection.
;; Keyword: file ivy swiper preview select selection
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (ivy "0.8.0"))
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

(require 'ivy)

(defgroup ivy-file-preview nil
  "Preview the current ivy file selection."
  :prefix "ivy-file-preview-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/ivy-file-preview"))

(defun ivy-file-preview--enable ()
  "Enable `ivy-file-preview'."
  )

(defun ivy-file-preview--disable ()
  "Disable `ivy-file-preview'."
  )

;;;###autoload
(define-minor-mode ivy-file-preview-mode
  "Minor mode 'ivy-file-preview-mode'."
  :global t
  :require 'ivy-file-preview
  :group 'ivy-file-preview
  (if ivy-file-preview-mode (ivy-file-preview--enable) (ivy-file-preview--disable)))

(provide 'ivy-file-preview)
;;; ivy-file-preview.el ends here
