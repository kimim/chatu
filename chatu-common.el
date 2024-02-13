;;; chatu-common.el --- Common functions for chatu-mode   -*- lexical-binding: t -*-

;; Copyright (c) 2024 Kimi Ma <kimi.im@outlook.com>

;; Author:  Kimi Ma <kimi.im@outlook.com>
;; URL: https://github.com/kimim/chatu
;; Keywords: multimedia convenience

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `chatu-common-open-other-window' to open file in Emacs.

;;; Code:

(defun chatu-common-open-other-window (keyword-plist file-ext)
  "Open input file.
KEYWORD-PLIST contains parameters from the chatu line and
FILE-EXT is the default extension."
  (let* ((input-path (plist-get keyword-plist :input-path))
         (path (if (file-name-extension input-path)
                   input-path
                 (file-name-with-extension input-path file-ext))))
    (find-file-other-window path)))

(defun chatu-common-open-external (keyword-plist file-ext executable)
  "Open chatu input file.
KEYWORD-PLIST contains parameters from the chatu line, FILE-EXT
is the default file extension, and EXECUTABLE is the program to
open the file."
  (let* ((input-path (plist-get keyword-plist :input-path))
         (path (if (file-name-extension input-path)
                   input-path
                 (file-name-with-extension input-path file-ext))))
    (cond
     ;; ensure that draw.io.exe is in execute PATH
     ((string-equal system-type "windows-nt")
      (if (fboundp 'w32-shell-execute)
          (w32-shell-execute "open" path)))
     ;; TODO: need some test for other systems
     ((string-equal system-type "darwin")
      (start-process "" nil "open" "-a" executable
                     path))
     ((string-equal system-type "gnu/linux")
      (start-process "" nil "xdg-open"
                     executable path))
     ((string-equal system-type "cygwin")
      (start-process "" nil "xdg-open"
                     executable path)))))

(provide 'chatu-common)
