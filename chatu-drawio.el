;;; chatu-drawio.el --- Chatu for drawio  -*- lexical-binding: t -*-

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

;; script and open function for drawio

;;; Code:

(require 'chatu-common)

(defcustom chatu-drawio-executable-func #'chatu-drawio--find-executable
  "The function to find the drawio executable."
  :group 'chatu
  :type 'function)

(defun chatu-drawio--find-executable ()
  "Find the drawio executable on PATH, or else return an error."
  (condition-case nil
      (file-truename (or (executable-find "draw.io")
                         ;; drawio on gentoo
                         (executable-find "drawio")))
    (wrong-type-argument
     (message "Cannot find the draw.io executable on the PATH."))))

(defun chatu-drawio-script (keyword-plist)
  "Get conversion script.
KEYWORD-PLIST contains parameters from the chatu line."
  (let* ((input-path
          (chatu-common-with-extension
           (plist-get keyword-plist :input-path) "drawio"))
         (output-ext (plist-get keyword-plist :output-ext))
         (output-path (plist-get keyword-plist :output-path))
         (output-path-pdf (file-name-with-extension output-path "pdf"))
         (page (plist-get keyword-plist :page))
         (drawio-path (shell-quote-argument (funcall chatu-drawio-executable-func))))
    (if output-ext
        (format "%s %s -f %s -x %s -p %s -o %s"
                drawio-path
                (if (plist-get keyword-plist :crop) "--crop" "")
                (shell-quote-argument output-ext)
                (shell-quote-argument input-path)
                (or page "0")
                (shell-quote-argument (file-name-with-extension output-path output-ext)))
      (if (plist-get keyword-plist :nopdf)
          (format "%s %s -x %s -p %s -o %s"
                  drawio-path
                  (if (plist-get keyword-plist :crop) "--crop" "")
                  (shell-quote-argument input-path)
                  (or page "0")
                  (shell-quote-argument output-path))
        (format "%s %s -x %s -p %s -o %s && %s %s %s && rm %s"
                drawio-path
                (if (plist-get keyword-plist :crop) "--crop" "")
                (shell-quote-argument input-path)
                (or page "0")
                (shell-quote-argument output-path-pdf)
                "pdf2svg"
                (shell-quote-argument output-path-pdf)
                (shell-quote-argument output-path)
                (shell-quote-argument output-path-pdf))))))

(defconst chatu-drawio-empty
  "<mxfile><diagram><mxGraphModel></mxGraphModel></diagram></mxfile>"
  "Content of empty drawio file.")

(defun chatu-drawio-open (keyword-plist)
  "Open .drawio file.
KEYWORD-PLIST contains parameters from the chatu line."
  (interactive)
  (let* ((path (plist-get keyword-plist :input-path))
         (path (file-truename (chatu-common-with-extension path "drawio")))
         (executable (chatu-drawio--find-executable)))
    (chatu-common-open-external executable path chatu-drawio-empty)))

(provide 'chatu-drawio)

;;; chatu-drawio.el ends here
