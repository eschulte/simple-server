;;; simpler-file-server.el --- serves files for simpler-server
;;
;; Copyright (C) 2008 Eric Schulte
;;
;; Author: Eric Schulte
;;
;; This file is not currently part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Serves files for `simpler-server'.  Use `ssfs-file-preprocessors'
;; to customize how files should be exported to html, otherwise files
;; will be exported using `htmlize'.

;;; Code:
(require 'simple-server)
(require 'htmlize)
(require 'org)
(require 'org-exp)

(defvar ssfs-document-root "/var/www/simple-server/")

(defvar ssfs-file-prefix ".ssfs.")

(defvar ssfs-uri-prefix "docs")

(defvar ssfs-read-directories t
  "Controls whether ssfs will render directories")

(defvar ssfs-file-prefix-regexp (concat ".*" (regexp-quote ssfs-file-prefix) ".*"))

(defmacro with-temp-filebuffer (file &rest body)
  "Open FILE into a temporary buffer execute BODY there like
`progn', then kill the FILE buffer returning the result of
evaluating BODY."
  (let ((temp-result (make-symbol "temp-result"))
	(temp-file (make-symbol "temp-file")))
    `(let (,temp-result ,temp-file)
       (find-file ,file)
       (setf ,temp-file (current-buffer))
       (setf ,temp-result (progn ,@body))
       (kill-buffer ,temp-file)
       ,temp-result)))

(defun ssfs-mime-type (filename)
  (or (cdr (assoc (file-name-extension filename) httpd-mime-types-alist))
      "text/plain"))

(defun ssfs-preprocess-dired-mode (html-path)
  (when ssfs-read-directories
    (set-buffer (htmlize-buffer))
    ;; make links of the file names
    (goto-char (point-min))
    (while (re-search-forward
	    "[[:digit:]]+:[[:digit:]]+ \\(<span class=\"dired-directory\">\\|\\)\\(.*?\\)\\(</span>\\|\\)$"
	    nil t)
      (replace-match (concat "<a href=\""
			     (concat (ss-request-uri) "/"
				     (save-match-data (match-string 2)))
			     "\">"
			     (save-match-data (match-string 2))
			     "</a>") t t nil 2))
    (write-file html-path)
    (kill-buffer)
    (cons "html" html-path)))

(defun ssfs-preprocess-org-mode (html-path)
  (save-window-excursion
    (org-export-as-html-to-buffer nil)
    (write-file html-path)
    (kill-buffer)
    (cons "html" html-path)))

(defun ssfs-preprocess-default (html-path)
  (set-buffer (htmlize-buffer))
  (write-file html-path)
  (kill-buffer)
  (cons "html" html-path))

(defun ssfs-preprocess (file-path)
  (let* ((file-name (file-name-nondirectory file-path))
	 (file-dir (file-name-directory file-path))
	 (html-path (expand-file-name (concat ssfs-file-prefix file-name) file-dir))
	 results)
    ;; check if exported since last change
    (if (and (file-exists-p html-path)
	     (< 0 (time-to-seconds
		   (time-subtract
		    (nth 5 (file-attributes html-path))
		    (nth 5 (file-attributes file-path))))))
	html-path
      (setf results
	    (save-excursion
	      (with-temp-filebuffer
	       file-path
	       (case major-mode
		 ;; if no mode activates, just return the raw text
		 (fundamental-mode (cons "text" file-path))
		 ;; if the path is a directory
		 (dired-mode (ssfs-preprocess-dired-mode html-path))
		 ;; org-mode exportation
		 (org-mode (ssfs-preprocess-org-mode html-path))
		 ;; most modes just use htmlize to export
		 (t (ssfs-preprocess-default html-path))))))
      (when results
	;; set the type
	(ss-set-header 'type (car results))
	;; return the path to the file
	(cdr results)))))

(defun ssfs-handler ()
  (let* ((uri (ss-request-uri))
	 (file (and (string-match (concat "^/" ssfs-uri-prefix "/\\(.*\\)$") uri)
		    (match-string 1 uri)))
	 (ext (and (file-name-extension file)))
	 (path (concat ssfs-document-root "/" file))
	 (found (file-exists-p path))
	 type)
    (when found
      (ss-set-header 'type 200)
      (ss-log path "file-path")
      ;; possibly preprocess file
      (setf type (car (assoc ext ss-mime-types)))
      (if type
	  (progn
	    (ss-set-header 'type (car (assoc ext ss-mime-types)))
	    (cond ;; TODO: find out how to stream audio
	     ((string-match "audio" (cdr (assoc ext ss-mime-types)))
	      (ss-set-header 'connection "keep-alive"))))
	(setf path (ssfs-preprocess path)))
      ;; send file
      (when path
	(with-temp-buffer
	  (insert-file-contents path)
	  (buffer-string))))))

(defun ssfs-dispatcher ()
  (if (string-match (concat "^/" ssfs-uri-prefix "/") (ss-request-uri))
      'ssfs-handler
    nil))

(setf ss-dispatch-table (cons 'ssfs-dispatcher ss-dispatch-table))

(provide 'simple-file-server)
;;; simpler-file-server.el ends here