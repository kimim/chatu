;;; chatu.el --- Convert and insert diagram to buffer -*- lexical-binding: t -*-

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

;;; Acknowledgements:

;; Inspired a lot by https://github.com/nobiot/org-transclusion from
;; Noboru Ota <me@nobiot.com>

;;; Commentary:

;; When you are drawing with draw.io or plantuml and you want to
;; insert the svg image exported from the file, the operation is
;; nontrivial.  This extension provide two commands to reduce the
;; manual work:
;;
;; - `chatu-add' convert diagram to svg, and insert it to buffer
;; - `chatu-open' open diagram file at the point

;;; Installation

;; To enable, add the following:
;;
;;   (use-package chatu
;;     :custom ((chatu-input-dir "./draws")
;;              (chatu-output-dir "./images")))
;;
;; You may need to install drawio, plantuml and pdf2svg in your PATH.

;;; Customization

;; `chatu-input-dir': diagram input folder, default is ./draws
;; `chatu-output-dir': default svg output folder, default is ./images
;; `chatu-dir-regex': customize folder regex
;; `chatu-file-regex': customize file name regex

;;; Change log

;; 2025/02/10
;;      * Initial version support draw.io and plantuml

;;; Code:

(defgroup chatu nil
  "Insert diagram as svg image."
  :group 'tools
  :group 'chatu
  :prefix "chatu-"
  :link '(url-link :tag "Github" "https://github.com/kimim/chatu")
  :package-version '("Chatu" . "1.0.0"))

(defcustom chatu-input-dir "./draws"
  "Define default input folder."
  :type 'string)

(defcustom chatu-output-dir "./images"
  "Define default output folder."
  :type 'string)

(defcustom chatu-file-regex
  "\"\\([\u4e00-\u9fa5:~ \\/a-z_\s0-9\\.-\\%]+\\)\""
  "Define regex of file. Currently support Chinese characters."
  :type 'string)

(defcustom chatu-dir-regex
  "\"\\([\u4e00-\u9fa5:~ \\/a-z_\s0-9\\.-]+\\)\""
  "Define regex of directry. Currently support Chinese charaters."
  :type 'string)

(defun chatu-get-type (line)
  "Get chatu type from string LINE."
  (when (string-match
         ":\\(\\w+\\) +" line)
    (list :type
          (substring-no-properties
           (match-string 1 line)))))

(defun chatu-get-input (line)
  "Get chatu input file from string LINE."
  (when (string-match
           (concat ":\\w* +" chatu-file-regex) line)
      (list :input
            (substring-no-properties
             (match-string 1 line)))))

(defun chatu-get-output (line)
  "Get chatu output file from string LINE."
  (when (string-match
           (concat ":output +" chatu-file-regex) line)
      (list :output
            (substring-no-properties
             (match-string 1 line)))))

(defun chatu-get-input-dir (line)
  "Get chatu output directory from string LINE."
  (when (string-match
           (concat ":input-dir +" chatu-dir-regex) line)
      (list :input-dir
            (substring-no-properties
             (match-string 1 line)))))

(defun chatu-get-output-dir (line)
  "Get chatu input directory from string LINE."
  (when (string-match
           (concat ":output-dir +" chatu-dir-regex) line)
      (list :output-dir
            (substring-no-properties
             (match-string 1 line)))))

(defun chatu-get-page (line)
  "Get chatu output page from string LINE."
  (when (string-match ":page +\\([0-9]*\\)[ \\t\\n]*" line)
    (list :page
          (substring-no-properties
           (match-string 1 line)))))

(defvar chatu-keyword-value-functions
      '(chatu-get-type
        chatu-get-input
        chatu-get-output
        chatu-get-page
        chatu-get-input-dir
        chatu-get-output-dir))

(defun chatu-normalize-keyword-plist (keyword-plist)
  "Normalize KEYWORD-PLIST."
  (let* ((input-dir (or (plist-get keyword-plist :input-dir)
                        chatu-input-dir))
         (input (plist-get keyword-plist :input))
         (_ (plist-put keyword-plist :input-path (concat input-dir "/" input)))
         (output-dir (or (plist-get keyword-plist :output-dir)
                         chatu-output-dir))
         (output-param (plist-get keyword-plist :output))
         (page (plist-get keyword-plist :page))
         (output (or output-param
                     (if page
                         (concat (file-name-sans-extension input) "-" page ".svg")
                       (file-name-with-extension input "svg"))))
         (_ (plist-put keyword-plist :output-path (concat output-dir "/" output))))
    keyword-plist))

(defun chatu-keyword-plist ()
  "Get normalized KEYWORD-PLIST from string line."
  (let ((plist))
    (when-let ((line (buffer-substring (line-beginning-position)
                                       (line-end-position))))
      (dolist (fn chatu-keyword-value-functions) plist
              (setq plist (append plist (funcall fn line)))))
    (chatu-normalize-keyword-plist plist)))

(defun chatu-refresh-image ()
  "Refresh image for different major mode."
  (cond ((and (featurep 'markdown-mode)
              (derived-mode-p 'markdown-mode))
         (markdown-remove-inline-images)
         (markdown-display-inline-images))
        ((and (featurep 'org-mode)
              (derived-mode-p 'org-mode)))
        ((org-redisplay-inline-images))))

(defun chatu-insert-image (path)
  "Insert image string PATH for different major mode."
  (cond ((derived-mode-p 'markdown-mode)
         (insert "![](" path ")"))
        ((derived-mode-p 'org-mode)
         (insert "[[file:" path "]]"))))

(defun chatu-img-pre ()
  "Get image string prefix for different major mode."
  (cond ((derived-mode-p 'markdown-mode)
         "![")
        ((derived-mode-p 'org-mode)
         "[[")))

(defun chatu-skip-lines ()
  "Skip lines after chatu line."
  (while (and (derived-mode-p 'org-mode)
              (string-prefix-p
               "#+" (buffer-substring
                     (line-beginning-position)
                     (line-end-position))))
    (forward-line)))

;;;###autoload
(defun chatu-add ()
  "Concert diagram to svg and add to buffer."
  (interactive)
  (save-excursion
    (let* ((keyword-plist (chatu-keyword-plist))
           (type (plist-get keyword-plist :type))
           (script (progn
                     (require (intern (concat "chatu-" type)))
                     (funcall (intern (concat "chatu-" type "-script"))
                              keyword-plist)))
           (result (plist-get keyword-plist :output-path)))
      (forward-line)
      (chatu-skip-lines)
      (let ((process (start-process-shell-command "chatu-buffer" nil script)))
        (set-process-sentinel
         process `(lambda (process event)
                    (when (string-match-p "finished" event)
                      ;; refresh image
                      (chatu-refresh-image)))))
      (if (string-prefix-p (chatu-img-pre)
                           (buffer-substring
                            (line-beginning-position)
                            (line-end-position)))
          ;; when it is image link
          (kill-whole-line 0)
        (progn (beginning-of-line) (open-line 1)))
      (chatu-insert-image result))))

;;;###autoload
(defun chatu-open ()
  "Open diagram file in emacs."
  (interactive)
  (let* ((keyword-plist (chatu-keyword-plist))
         (type (plist-get keyword-plist :type)))
    (require (intern (concat "chatu-" type)))
    (funcall (intern (concat "chatu-" type "-open"))
             keyword-plist)))

(provide 'chatu)

;;; chatu.el ends here
