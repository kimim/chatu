;;; chatu.el --- Convert and insert any images to org-mode or markdown buffer -*- lexical-binding: t -*-

;; Copyright (c) 2024 Kimi Ma <kimi.im@outlook.com>

;; Author:  Kimi Ma <kimi.im@outlook.com>
;; URL: https://github.com/kimim/chatu
;; Keywords: multimedia convenience
;; Version: 0.1
;; Package-Requires: ((org "9.6.6") (emacs "29.1") (plantuml-mode "1.2.9"))

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
;; - `chatu-new' add a chatu line
;; - `chatu-add' convert diagram to svg, and insert it to buffer
;; - `chatu-open' open diagram file at the point
;;
;; When `chatu-mode' is activated,
;; in `org-mode',
;; - "C-c C-o" on chatu line will invoke `chatu-open'
;; - "C-c C-c" on chatu line will invoke `chatu-add'
;;
;; in `markdown-mode'
;; - "C-c C-o" on chatu line will invoke `chatu-open'
;; - "C-c C-c C-c" on chatu line will invoke `chatu-add'

;;; Installation:

;; To enable, add the following:
;;
;;   (use-package chatu
;;     :custom ((chatu-input-dir "./draws")
;;              (chatu-output-dir "./images"))
;;     :hook ((org-mode markdown-mode) . chatu-mode))

;;; Customization:

;; `chatu-input-dir': diagram input folder, default is ./draws
;; `chatu-output-dir': default svg output folder, default is ./draws_out
;; `chatu-dir-regex': customize folder regex
;; `chatu-file-regex': customize file name regex

;;; Change log:

;; 2024/03/03
;;      * added to elpa package, thanks the review support from @riscy
;;
;; 2024/03/02
;;      * add new chatu option `:output-ext' to set output extension.
;;
;; 2024/02/21
;;      * add `declare-function' as suggested by riscy.
;;
;; 2024/02/13
;;      * Support curl, babashka, clojure, R, LilyPond
;;
;; 2024/02/10
;;      * Initial version support draw.io and plantuml

;;; Code:

(defgroup chatu nil
  "Insert diagram as svg image."
  :group 'tools
  :group 'chatu
  :prefix "chatu-"
  :link '(url-link :tag "Github" "https://github.com/kimim/chatu")
  :package-version '("Chatu" . "1.0.0"))

(declare-function markdown-remove-inline-images "ext:markdown-mode.el")
(declare-function markdown-display-inline-images "ext:markdown-mode.el")
(declare-function markdown-follow-thing-at-point "ext:markdown-mode.el")
(declare-function org-redisplay-inline-images "org.el")

(defvar org-ctrl-c-ctrl-c-hook)
(defvar markdown-mode-map)
(defvar org-open-at-point-functions)

(defcustom chatu-input-dir "./draws"
  "Define default input folder."
  :type 'string)

(defcustom chatu-output-dir "./draws_out"
  "Define default output folder."
  :type 'string)

(defcustom chatu-file-regex
  "\"\\([\u4e00-\u9fa5:~ \\/a-z_\s0-9\\.\\%-]+\\)\""
  "Define regex of file. Currently support Chinese characters."
  :type 'string)

(defcustom chatu-dir-regex
  "\"\\([\u4e00-\u9fa5:~ \\/a-z_\s0-9\\.-]+\\)\""
  "Define regex of directry. Currently support Chinese charaters."
  :type 'string)

(defun chatu-get-keyword (line)
  "Get chatu keyword from string LINE."
  (when (and
         (string-match
          "#\\+\\(\\w+\\): +" line)
         (string= "chatu"
                  (substring-no-properties
                   (match-string 1 line))))
    (list :chatu t)))

(defun chatu-get-type (line)
  "Get chatu type from string LINE."
  (when (string-match
         ":\\(\\w+\\) +" line)
    (list :type
          (substring-no-properties
           (match-string 1 line)))))

(defun chatu-get-settings (line)
  "Get all chatu settings from string LINE."
  (save-match-data
    (let ((pos 0)
          settings)
      (while (string-match "\\(:\\w+\\)\\( +:\\|$\\)" line pos)
        (setq settings
              (append settings
                      (list
                       (intern
                        (substring-no-properties
                         (match-string 1 line)))
                       t)))
        (setq pos (match-end 1)))
      settings)))

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

(defun chatu-get-output-ext (line)
  "Get chatu output file extension from string LINE."
  (when (string-match
         ":output-ext +\\(.+\\)[ \\t\\n]*" line)
    (list :output-ext
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

(defun chatu-get-script (line)
  "Get chatu inline script from string LINE."
  (when (string-match ":script +\"\\(.*\\)\"[ \\t\\n]*" line)
    (list :script
          (substring-no-properties
           (match-string 1 line)))))

(defvar chatu-keyword-value-functions
      '(chatu-get-keyword
        chatu-get-settings
        chatu-get-type
        chatu-get-input
        chatu-get-output
        chatu-get-page
        chatu-get-input-dir
        chatu-get-output-dir
        chatu-get-script
        chatu-get-output-ext))

(defun chatu-normalize-keyword-plist (keyword-plist)
  "Normalize KEYWORD-PLIST."
  (when (plist-get keyword-plist :chatu)
      (let* ((input (plist-get keyword-plist :input))
             (input-dir (or (plist-get keyword-plist :input-dir)
                            ;; if input already contains parent folder
                            ;; ignore `chatu-input-dir'
                            (if (file-name-directory input)
                                nil
                              chatu-input-dir)))
             (_ (plist-put keyword-plist :input-path
                           (if input-dir
                               (concat input-dir "/" input)
                             input)))
             (output-ext (plist-get keyword-plist :output-ext))
             (output (plist-get keyword-plist :output))
             (output-dir (or (plist-get keyword-plist :output-dir)
                            ;; if output already contains parent folder
                            ;; ignore `chatu-output-dir'
                             (if (and output (file-name-directory output))
                                 nil
                               chatu-output-dir)))
             (page (plist-get keyword-plist :page))
             (output (or output
                         (if page
                             (concat (file-name-sans-extension
                                      ;; remove input's parent folder
                                      (file-name-base input))
                                     "-" page "." (or output-ext "svg"))
                           (file-name-with-extension
                            (file-name-base input)
                            ;; when output-ext is set, use it.
                            (or output-ext "svg")))))
             (_ (plist-put keyword-plist :output-path
                           (if output-dir
                               (concat output-dir "/" output)
                             output))))
        keyword-plist)))

(defun chatu-keyword-plist ()
  "Get normalized KEYWORD-PLIST from string line."
  (let ((plist))
    (when-let ((line (buffer-substring (line-beginning-position)
                                       (line-end-position))))
      (dolist (fn chatu-keyword-value-functions) plist
              (setq plist (append plist (funcall fn line)))))
    (chatu-normalize-keyword-plist plist)))

(defun chatu-refresh-image ()
  "Refresh image depending on the current major mode."
  (cond
   ((derived-mode-p 'markdown-mode)
    (markdown-remove-inline-images)
    (markdown-display-inline-images))
   ((derived-mode-p 'org-mode)
    (org-redisplay-inline-images))))

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
(defun chatu-new ()
  "Insert chatu text line respect to mode."
  (interactive)
  (insert
   (cond ((derived-mode-p 'markdown-mode)
          "<!-- #+chatu: :")
         ((derived-mode-p 'org-mode)
          "#+chatu: :"))
   (read-string "type: " "drawio")
   " \""
   (read-string "input name: " "")
   "\""
   (if (derived-mode-p 'markdown-mode)
       " -->"
     "\n#+results:"))
  (when (derived-mode-p 'org-mode)
    (forward-line -1)))

;;;###autoload
(defun chatu-add ()
  "Concert diagram to svg and add to buffer."
  (interactive)
  (save-excursion
    (let* ((keyword-plist (chatu-keyword-plist))
           (type (downcase
                  (plist-get keyword-plist :type)))
           (script (progn
                     (require (intern (concat "chatu-" type)))
                     (funcall (intern (concat "chatu-" type "-script"))
                              keyword-plist)))
           ;; ~ is after `shell-quote-argument' is \~, which is not
           ;; working. remove it.
           (script (string-replace "\\~" "~" script))
           (result (plist-get keyword-plist :output-path))
           (result-dir (file-name-directory result)))
      ;; ensure output-dir exists.
      (when (not (file-exists-p result-dir))
        (make-directory result-dir t))
      (forward-line)
      (chatu-skip-lines)
      (let ((process (start-process-shell-command "chatu-buffer" nil script)))
        (set-process-sentinel
         process `(lambda (process event)
                    ;; refresh image
                    (chatu-refresh-image))))
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
  "Open diagram file in Emacs."
  (interactive)
  (let* ((keyword-plist (chatu-keyword-plist))
         (type (downcase
                (plist-get keyword-plist :type))))
    (require (intern (concat "chatu-" type)))
    (funcall (intern (concat "chatu-" type "-open"))
             keyword-plist)))

(defun chatu-ctrl-c-ctrl-c ()
  "Hook function for `org-ctrl-c-ctrl-c-hook'."
  (let ((plist (chatu-keyword-plist)))
    (if (plist-get plist :chatu)
        (progn
          (chatu-add)
          t)
      nil)))

(defun chatu-ctrl-c-ctrl-o (&optional args)
  "Hook function for `org-open-at-point-functions'.
ARGS is ignored, required by `markdown-follow-thing-at-point'."
  (ignore args) ;; args are not used.
  (let ((plist (chatu-keyword-plist)))
    (if (plist-get plist :chatu)
        (progn
          (chatu-open)
          t)
      nil)))

;;;###autoload
(define-minor-mode chatu-mode
  "Add chatu commands or hooks."
  :global nil
  :lighter " Chatu"
  (if (not chatu-mode)
      (cond ((derived-mode-p 'markdown-mode)
             (keymap-unset markdown-mode-map "C-c C-c C-c" t)
             (advice-remove 'markdown-follow-thing-at-point
                            #'chatu-ctrl-c-ctrl-o))
            ((derived-mode-p 'org-mode)
             (remove-hook 'org-ctrl-c-ctrl-c-hook #'chatu-ctrl-c-ctrl-c)
             (setq org-open-at-point-functions
                   (delete 'chatu-ctrl-c-ctrl-o org-open-at-point-functions))))
    (cond ((derived-mode-p 'markdown-mode)
           (keymap-set markdown-mode-map "C-c C-c C-c" 'chatu-add)
           (advice-add 'markdown-follow-thing-at-point
                       :before-until
                       #'chatu-ctrl-c-ctrl-o))
          ((derived-mode-p 'org-mode)
           (add-hook 'org-ctrl-c-ctrl-c-hook #'chatu-ctrl-c-ctrl-c)
           (add-to-list 'org-open-at-point-functions #'chatu-ctrl-c-ctrl-o)))))

(provide 'chatu)

;;; chatu.el ends here
