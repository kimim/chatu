;;; chatu-r.el --- Chatu for R  -*- lexical-binding: t -*-

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

;; script and open function for R

;;; Code:

(defun chatu-r-script (keyword-plist)
  "Open input.
KEYWORD-PLIST contains parameters from the chatu line."
  (let* ((input-path (plist-get keyword-plist :input-path))
         (path (if (file-name-extension input-path)
                   input-path
                 (file-name-with-extension input-path "R"))))
    ;; TODO: how to set image output path for Rscript?
    (format "Rscript %s"
            (shell-quote-argument path))))

(defun chatu-r-open (keyword-plist)
  "Open input file.
KEYWORD-PLIST contains parameters from the chatu line."
  (interactive)
  (let* ((input-path (plist-get keyword-plist :input-path))
         (path (if (file-name-extension input-path)
                   input-path
                 (file-name-with-extension input-path "R"))))
    ;; TODO: how to set image output path for Rscript?
    (find-file-other-window path)))

(provide 'chatu-r)

;;; chatu-r.el ends here

(file-name-extension "hello/hello")
