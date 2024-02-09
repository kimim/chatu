;;; chatu-drawio.el --- Chatu for drawio  -*- lexical-binding: t -*-

;; Copyright (c) 2024 Kimi Ma <kimi.im@outlook.com>

;; Author:  Kimi Ma <kimi.im@outlook.com>
;; URL: https://github.com/kimim/chatu
;; Keywords: multimedia convenience
;; Version: 0.1
;; Package-Requires: ((org "9.6.6") (emacs "28.1"))

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

;; script and open function for drawio

;;; Code:

(defun chatu-drawio-add-extention (path)
  "Change to default extension for PATH of the input file."
  (file-name-with-extension path "drawio"))

(defun chatu-drawio-script (keyword-plist)
  "Get conversion script. KEYWORD-PLIST contains parameters from
 the chatu line."
  (let* ((input-path
          (chatu-drawio-add-extention
           (plist-get keyword-plist :input-path)))
         (output-path (plist-get keyword-plist :output-path))
         (output-path-pdf (file-name-with-extension output-path "pdf"))
         (page (plist-get keyword-plist :page)))
    (format "%s -x %s -p %s -o %s >/dev/null 2>&1 && \
%s %s %s >/dev/null 2>&1 && rm %s"
            "draw.io"
            (shell-quote-argument input-path)
            (or page "0")
            (shell-quote-argument output-path-pdf)
            "pdf2svg"
            (shell-quote-argument output-path-pdf)
            (shell-quote-argument output-path)
            (shell-quote-argument output-path-pdf))))

(defun chatu-drawio-open (keyword-plist)
  "Open .drawio file. KEYWORD-PLIST contains parameters from the
 chatu line."
  (interactive)
  (let ((path (chatu-drawio-add-extention
               (plist-get keyword-plist :input-path))))
    (cond
     ;; ensure that draw.io.exe is in execute PATH
     ((string-equal system-type "windows-nt")
      (if (fboundp 'w32-shell-execute)
          (w32-shell-execute "open" path)))
     ;; TODO: need some test for other systems
     ((string-equal system-type "darwin")
      (shell-command (concat "draw.io"
                             (shell-quote-argument
                              (format " %s" path)))))
     ((string-equal system-type "gnu/linux")
      (start-process "" nil "xdg-open"
                     "draw.io"
                     path))
     ((string-equal system-type "cygwin")
      (start-process "" nil "xdg-open" "draw.io"
                     path)))))

(provide 'chatu-drawio)

;;; chatu-drawio.el ends here
