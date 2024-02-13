;;; chatu-plantuml.el --- Chatu for plantuml  -*- lexical-binding: t -*-

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

;; script and open function for plantuml

;;; Code:

(require 'chatu-common)
(require 'plantuml-mode)

(defun chatu-plantuml-add-extention (path)
  "Change to default extension for PATH of the input file."
  (file-name-with-extension path "puml"))

(defun chatu-plantuml-script (keyword-plist)
  "Get conversion script.
KEYWORD-PLIST contains parameters from the chatu line."
  (let ((page (plist-get keyword-plist :page)))
    (concat "java -jar "
            plantuml-jar-path
            " -p -tsvg " (when page (concat "-pipeimageindex " page))
            " < "
            (chatu-plantuml-add-extention
             (plist-get keyword-plist :input-path))
            " > "
            (plist-get keyword-plist :output-path))))

(defun chatu-plantuml-open (keyword-plist)
  "Open .puml file.
KEYWORD-PLIST contains parameters from the chatu line."
  (interactive)
  (chatu-common-open-other-window keyword-plist "puml"))

(provide 'chatu-plantuml)

;;; chatu-plantuml.el ends here
