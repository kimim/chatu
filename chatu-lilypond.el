;;; chatu-lilypond.el --- Chatu for LilyPond  -*- lexical-binding: t -*-

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

;; script and open function for LilyPond

;;; Code:

(require 'chatu-common)

(defcustom chatu-lilypond-empty
  "\\header {
  title = \"\"
}
\\relative c' {
}"
  "Content of empty LilyPond file."
  :group 'chatu
  :type 'string)

(defun chatu-lilypond-script (keyword-plist)
  "Open input.
KEYWORD-PLIST contains parameters from the chatu line."
  (let* ((input-path (plist-get keyword-plist :input-path))
         (input-path (chatu-common-with-extension input-path "ly"))
         (output-sans-ext (file-name-sans-extension
                           (plist-get keyword-plist :output-path)))
         (with-svg (file-name-with-extension output-sans-ext "svg"))
         (cropped (concat output-sans-ext ".cropped.svg")))
    (format "lilypond -dcrop --loglevel=ERROR --svg --output=%s %s && mv -f %s %s"
            (shell-quote-argument output-sans-ext)
            (shell-quote-argument input-path)
            (shell-quote-argument cropped)
            (shell-quote-argument with-svg))))


(defun chatu-lilypond-open (keyword-plist)
  "Open input file.
KEYWORD-PLIST contains parameters from the chatu line."
  (interactive)
  (let* ((path (plist-get keyword-plist :input-path))
         (path (chatu-common-with-extension path "ly")))
    (chatu-common-open-other-window path chatu-lilypond-empty)))

(provide 'chatu-lilypond)

;;; chatu-lilypond.el ends here
