;;; chatu-clojure.el --- Chatu for Clojure  -*- lexical-binding: t -*-

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

;; script and open function for Clojure

;;; Code:

(require 'chatu-common)

(defun chatu-clojure-script (keyword-plist)
  "Open input.
KEYWORD-PLIST contains parameters from the chatu line."
  (let* ((path (plist-get keyword-plist :input-path))
         (path (chatu-common-with-extension path "clj")))
    (format "clojure -M %s"
            (shell-quote-argument path))))

(defun chatu-clojure-open (keyword-plist)
  "Open input file.
KEYWORD-PLIST contains parameters from the chatu line."
  (interactive)
  (let* ((path (plist-get keyword-plist :input-path))
         (path (chatu-common-with-extension path "clj")))
    (chatu-common-open-other-window path "")))

(provide 'chatu-clojure)

;;; chatu-clojure.el ends here
