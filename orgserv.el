;; orgserv.el --- serve/edit org-mode files through the web
;; 
;; Author: Eric Schulte
;; 
(require 'org)
(require 'gnuserv)

(defvar orgserv-base
  (file-name-as-directory
   (expand-file-name "www" (file-name-directory (or load-file-name buffer-file-name))))
  "base directory from which to serve/store/find web-pages")

(defun orgserv-file-to-html (path)
  "Return the supplied org-mode file as html"
  (let ((default-directory orgserv-base))
    (when (find-file path)
      (org-export-as-html nil t nil 'string))))

(defun orgserv-subtree-to-html (path id)
  "Export the subtree located in the file at PATH with the id ID
to html and return the resulting string"
  (let ((default-directory orgserv-base)
	position html)
    (save-excursion
      (when (and (find-file path) (setf position (org-find-entry-with-id id)))
	(goto-char position)
	(org-narrow-to-subtree)
	(setf html (org-export-as-html nil t nil 'string t))
	(kill-buffer)))
    ;; wrap the resulting html in AJAXyness
    
    html
    
    ))

(defun orgserv-org-string-to-html (string)
  "Accept a org-mode formatted string, and return it as html."
  (with-temp-buffer
    (insert string)
    (org-export-as-html nil t nil 'string)))

(defun orgserv-start ()
  "attempting to implement a persistent loop that can accept
input through gnuclient, and return output there all in a
persistent process."
  ;; may not be possible
  )

;;--------------------------------------------------------------------------------
;; functions for serving subtrees independently

(defun orgserv-idify ()
  "assign each subtree in the current file a uid"
  (org-map-entries 'org-id-get-create))
