;;; chatu-latex.el --- Chatu for LaTeX  -*- lexical-binding: t -*-

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

;; script and open function for LaTeX

;;; Code:

(require 'chatu-common)

(defun chatu-latex-script (keyword-plist)
  "Open input.
KEYWORD-PLIST contains parameters from the chatu line."
  (let* ((input-path (plist-get keyword-plist :input-path))
         (input-path (chatu-common-with-extension input-path "tex"))
         (output-ext (plist-get keyword-plist :output-ext))
         (output-sans-ext (file-name-sans-extension
                           (plist-get keyword-plist :output-path)))
         (output-dir (file-name-directory output-sans-ext))
         (path-with-pdf (file-name-with-extension output-sans-ext "pdf"))
         (path-with-svg (file-name-with-extension output-sans-ext "svg")))
    (concat (format "xelatex -shell-escape -output-directory=%s %s"
                    (shell-quote-argument output-dir)
                    (shell-quote-argument input-path))
            (if (string= output-ext "svg")
                (format " && pdf2svg %s %s"
                        (shell-quote-argument path-with-pdf)
                        (shell-quote-argument path-with-svg))))))

(defconst chatu-latex-empty
  "\\\documentclass[preview]{standalone}
\\usepackage{tikz}
\\begin{document}
\\begin{tikzpicture}
  \\draw[draw=none, fill=white] (-9,-1) rectangle (9,5);
\\end{tikzpicture}
\\end{document}"
  "Content of empty standalone LaTeX tikz preview file.")

(defun chatu-latex-open (keyword-plist)
  "Open input file.
KEYWORD-PLIST contains parameters from the chatu line."
  (interactive)
  (let* ((path (plist-get keyword-plist :input-path))
         (path (chatu-common-with-extension path "tex")))
    (chatu-common-open-other-window path chatu-latex-empty)))

(provide 'chatu-latex)

;;; chatu-latex.el ends here
