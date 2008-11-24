;;; simple-server.el --- a SIMPLE web-server run by emacs
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

;;; Attribution (read: unwitting authors) :
;;
;; This borrows heavily from the following
;;
;; EmacsEchoServer on the Emacs Wiki at
;; http://www.emacswiki.org/emacs/EmacsEchoServer
;;
;; httpd.el by Eric Marsden and John Wiegley
;; http://www.emacswiki.org/emacs/HttpServer
;;
;; hunchentoot a common lisp web-server
;; http://weitz.de/hunchentoot/

;;; Commentary:
;;
;; `simple-server' implements a very simple http server which will
;; hopefully be able to hand off GET requests to functions specified
;; in the `ss-dispatch-table'.
;;
;; The `ss-dispatch-table' should contain a list of dispatch functions
;; each of which will be tried in turn and given a chance to return
;; either a handler or nil.  The dispatch functions will have access
;; to the`ss-request' variable through the following functions
;; 
;; - `ss-request-host' returns the host name of the request
;; - `ss-request-method' returns the method as a keyword i.e. :get
;; - `ss-request-uri' returns the path of the uri following the
;;    hostname and port etc...
;; 
;; handlers
;; 
;; A handler is a function which will be called with no arguments (but
;; does have access to the request functions mentioned above) and
;; should return a HTTP string which will be passed back to the
;; client.
;; 
;; Hopefully one day it will parse get parameters and handle post
;; requests.

;;; Code:
;; require statements
(defvar ss-clients '() 
  "alist where KEY is a client process and VALUE is the string")

(defvar ss-name "web-server")

(defvar ss-buffer "*web-server*")

(defvar ss-log "*simple-server-log*")

(defvar ss-host 'local)

(defvar ss-default-404-page
  "<html><head><title>404</title></head><body><center><h2>404 Not Found</h2></center></body></html>")

(defvar ss-request
  `((host . ,ss-host)
    (method . nil)
    (uri . nil))
  "The request object which will be set by each incomming http
request, and should only be accessed through the
`ss-request-host', `ss-request-method', and `ss-request-uri'
functions." )

(defvar ss-http-headers
  '((status . 200)
    (connection . "close")
    (type . "html")
    (server . "Emacs/simple-server.el"))
  "Alist of http headers")

(defvar ss-http-status-codes
  '((200 . "OK")
    (301 . "Moved permanently")
    (302 . "Moved temporarily")
    (400 . "Bad request")
    (403 . "Forbidden")
    (404 . "Not found")
    (405 . "Method not allowed")
    (500 . "Internal server error")
    (501 . "Not implemented")
    (503 . "Service unavailable"))
  "Alist of http status codes and their meanings")

(defvar ss-mime-types
  '(("html" . "text/html; charset=utf-8")
    ("txt"  . "text/plain; charset=utf-8")
    ("css"  . "text/css")
    ("jpg"  . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif"  . "image/gif")
    ("png"  . "image/png")
    ("tif"  . "image/tiff")
    ("tiff" . "image/tiff")
    ("gz"   . "application/octet-stream")
    ("ps"   . "application/postscript")
    ("pdf"  . "application/pdf")
    ("eps"  . "application/postscript")
    ("tar"  . "application/x-tar")
    ("rpm"  . "application/x-rpm")
    ("zip"  . "application/zip")
    ("mp3"  . "audio/mpeg")
    ("mp2"  . "audio/mpeg")
    ("mid"  . "audio/midi")
    ("midi" . "audio/midi")
    ("wav"  . "audio/x-wav")
    ("au"   . "audio/basic")
    ("ram"  . "audio/pn-realaudio")
    ("ra"   . "audio/x-realaudio")
    ("mpg"  . "video/mpeg")
    ("mpeg" . "video/mpeg")
    ("qt"   . "video/quicktime")
    ("mov"  . "video/quicktime")
    ("avi"  . "video/x-msvideo")))

(defvar ss-default-page
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
	\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
<head><title>Simple Server is up and running</title></head>
<body><h2>Simple Server</h2>
<p><b>Simple Server</b> is a VERY simple web-server written entirely<br>
in emacs lisp.  This is made possible thanks to the addition of<br>
server-sockets to emacs via the `make-network-process' command.<br></p>
<p>I would not recommend actually using this in anything even<br>
resembling a production environment.</p></body></html>\n")

(defvar ss-dispatch-table
  '((lambda ()
      (if (string-equal "/" (ss-request-uri)) (lambda () ss-default-page) nil)))
  "List of functions which accept the route of the request, and
then return a page.  If a function returns nil, then the next
function on the list is called.  These functions are tried in
order.  The last function on the list should render the default
page.")

;;; Control Functions:
(defun simple-server-start ()
  "Starts a simple emacs webserver"
  (interactive)
  (unless (process-status ss-name)
    (make-network-process :name ss-name
			  :host ss-host
			  :buffer ss-buffer
			  :family 'ipv4
			  :service 3000
			  :sentinel 'ss-sentinel
			  :filter 'ss-filter
			  :server 't) 
    (setq echo-server-clients '())))

(defun simple-server-stop ()
  "stop an emacs simple server"
  (interactive)
  (while  ss-clients
    (delete-process (car (car ss-clients)))
    (setq ss-clients (cdr ss-clients)))
  (delete-process ss-name))

(defun simple-server-toggle ()
  "turn the simple-server on or off"
  (interactive)
  (if (process-status ss-name) (simple-server-stop) (simple-server-start)))

(defun simple-server-restart ()
  "restart the emacs simple server"
  (interactive) (simple-server-stop) (simple-server-start))

;;; Process Implementation:
(defun ss-filter (proc string)   
  (let ((pending (assoc proc ss-clients))
        message index responce)
    ;;create entry if required
    (unless pending
      (setq ss-clients (cons (cons proc "") ss-clients))
      (setq pending  (assoc proc ss-clients)))
    (setq message (concat (cdr pending) string))
    (while (setq index (string-match "\n" message)) ;; this is the main server loop
      ;; collect the request
      (setq index (1+ index))
      (ss-log  (substring message 0 index) "in" proc)
      ;; find the route
      (when (string-match "GET \\([^ ]+\\) HTTP" message)
	(setf (cdr (assoc 'uri ss-request)) (match-string 1 message))
	(ss-log (ss-request-uri) "uri"))
      ;; when the request ends reply
      (when (string-equal "\r\n" message)
	(process-send-string proc (ss-serve))
	(unless (string= "keep-alive" (ss-get-header 'connection))
	  (delete-process proc)))
      (setq message (substring message index)))
    (setcdr pending message)))

(defun ss-sentinel (proc msg)
  (delq proc ss-clients)
  (ss-log (format "client %s has quit" proc) "quit"))

(defun ss-log (string type &optional client)
  "If a *simple-server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer ss-log)
      (with-current-buffer ss-log
	(goto-char (point-max))
	(insert type
		(if client (format " %s:" client) " ")
		string)
	(or (bolp) (newline)))))

(defun ss-create-log ()
  (interactive)
  (get-buffer-create ss-log))

;;; Utility Functions:
(defun ss-request-host ()
  "returns the host name of the request"
  (cdr (assoc 'host ss-request)))

(defun ss-request-method ()
  "returns the method as a keyword i.e. :get"
  (cdr (assoc 'method ss-request)))

(defun ss-request-uri ()
  "returns the path of the uri following the"
  (cdr (assoc 'uri ss-request)))

(defun ss-set-header (header value)
  (let ((pair (assoc header ss-http-headers)))
    (setf (cdr pair) value)))

(defun ss-reset-http-headers ()
  (mapcar (lambda (pair)
	    (ss-set-header (car pair) (cdr pair)))
	  '((status . 200)
	    (connection . "close")
	    (type . "html")
	    (server . "Emacs/simple-server.el"))))

(defun ss-get-header (header)
  (cdr (assoc header ss-http-headers)))

(defun ss-expand-status (code)
  (cdr (assoc code ss-http-status-codes)))

(defun ss-expand-mime (mime-type)
  (cdr (assoc mime-type ss-mime-types)))

(defun ss-http-header (&optional content-length)
  "Return a string of the http header based upon the contents of
`ss-http-headers'"
  (let* ((endl "\r\n")
	 (status (ss-get-header 'status))
	 (long-status (ss-expand-status status))
	 (status-str (int-to-string status))
	 (connection (ss-get-header 'connection))
	 (type (ss-get-header 'type))
	 (long-type (ss-expand-mime type))
	 (server (ss-get-header 'server))
	 (date (format-time-string "%a, %d %b %Y %H:%M:%S %Z")))
    (concat
     "HTTP/1.1 " status-str " " long-status endl
     "Connection: " connection endl
     "Date: " date endl
     "Status: " status-str " " long-status endl
     "Server: " server endl
     "Content-Type: " long-type endl
     (if content-length (format "Content-Length: %d%s" content-length endl))
     endl)))

(defun ss-serve ()
  "This function cycles through the functions in the
`ss-dispatch-table' until one returns a handler.  The handler is
then called and the result is wrapped in header information and
passed to the http client.

Default header information is specified in `ss-reset-http-headers'.
To change header information a handler should change the values
of `ss-http-headers' using `ss-set-header'."
  (let (handler result)
    (dolist (dispatcher ss-dispatch-table)
      (when (null handler)
	(setf handler (funcall dispatcher))))
    (ss-reset-http-headers)
    (if (and handler (setf result (funcall handler)))
	(progn
	  (ss-log (ss-http-header (length result)) "header:\n")
	  (concat (ss-http-header (length result)) result))
      (concat (and (ss-set-header 'status 404) (ss-http-header) ss-default-404-page)))))

(provide 'simple-server)
;;; simple-server.el ends here