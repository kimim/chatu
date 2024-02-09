;;; chatu-curl.el --- Chatu for curl  -*- lexical-binding: t -*-

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

;; script and open function for curl

;;; Code:

(defun chatu-curl-script (keyword-plist)
  "Get conversion script.
KEYWORD-PLIST contains parameters from the chatu line."
  (let* ((input (plist-get keyword-plist :input))
         (output-path (plist-get keyword-plist :output-path)))
    (format "curl -o %s %s"
            (shell-quote-argument output-path)
            (shell-quote-argument input))))

(defun chatu-curl-open (keyword-plist)
  "Open curl link.
KEYWORD-PLIST contains parameters from the chatu line."
  (interactive)
  (let ((link (plist-get keyword-plist :input)))
    (cond
     ;; ensure that draw.io.exe is in execute PATH
     ((string-equal system-type "windows-nt")
      (if (fboundp 'w32-shell-execute)
          (w32-shell-execute "open" link)))
     ;; TODO: need some test for other systems
     )))

(provide 'chatu-curl)

;;; chatu-curl.el ends here
