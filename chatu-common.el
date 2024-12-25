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

;; `chatu-common-with-extension' to add file extension.
;; `chatu-common-open-external' to open input file in system.

;;; Code:

(defun chatu-common-with-extension (path file-ext)
  "Add FILE-EXT to PATH if PATH has no extension."
  (if (file-name-extension path)
      path
    (file-name-with-extension path file-ext)))

(defun chatu-common-open-other-window (path empty)
  "Open chatu PATH in other window.
Fill PATH with EMPTY string if nonexist."
  (let ((parent (file-name-parent-directory path)))
    (when (not (file-exists-p parent))
      (make-directory parent t))
    (when (not (file-exists-p path))
      (write-region empty nil path))
    (find-file-other-window path)))

(defun chatu-common-open-external (executable path empty)
  "Open chatu PATH with EXECUTABLE program.
Fill PATH with EMPTY string, if nonexist."
  (let ((parent (file-name-parent-directory path)))
    (when (not (file-exists-p parent))
      (make-directory parent t))
    (when (not (file-exists-p path))
      (write-region empty nil path))
    (cond
     ;; ensure that draw.io.exe is in execute PATH
     ((string-equal system-type "windows-nt")
      (if (fboundp 'w32-shell-execute)
          (w32-shell-execute "open" path)))
     ;; TODO: need some test for other systems
     ((string-equal system-type "darwin")
      (start-process "" nil "open" "-a" executable path))
     ((string-equal system-type "gnu/linux")
      (start-process "" nil executable path))
     ((string-equal system-type "cygwin")
      (start-process "" nil "xdg-open"
                     executable path)))))

(provide 'chatu-common)

;;; chatu-common.el ends here
