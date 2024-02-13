;;; chatu-babashka.el --- Chatu for babashka  -*- lexical-binding: t -*-

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

;; script and open function for babashka

;;; Code:

(require 'chatu-common)

(defun chatu-babashka-script (keyword-plist)
  "Open input.
KEYWORD-PLIST contains parameters from the chatu line."
  (let ((input-path (plist-get keyword-plist :input-path)))
    (format "bb %s"
            (shell-quote-argument input-path))))

(defun chatu-babashka-open (keyword-plist)
  "Open input file.
KEYWORD-PLIST contains parameters from the chatu line."
  (interactive)
  (chatu-common-open-other-window keyword-plist "bb"))

(provide 'chatu-babashka)

;;; chatu-babashka.el ends here
