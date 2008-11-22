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

(defvar ss-request
  `((host . ,ss-host)
    (method . nil)
    (uri . nil))
  "The request object which will be set by each incomming http
request, and should only be accessed through the
`ss-request-host', `ss-request-method', and `ss-request-uri'
functions." )

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

;;; Implementation:
(defun ss-filter (proc string)   
  (let ((pending (assoc proc ss-clients))
        message index)
    ;;create entry if required
    (unless pending
      (setq ss-clients (cons (cons proc "") ss-clients))
      (setq pending  (assoc proc ss-clients)))
    (setq message (concat (cdr pending) string))
    (while (setq index (string-match "\n" message)) ;; this is the main server loop
      ;; collect the request
      (setq index (1+ index))
      (ss-log  (substring message 0 index) "out" proc)
      ;; find the route
      (when (string-match "GET \\([^ ]+\\) HTTP" message)
	(setf (cdr (assoc 'uri ss-request)) (match-string 1 message))
	(ss-log (ss-request-uri) "uri"))
      ;; when the request ends reply
      (when (string-equal "\r\n" message)
	(process-send-string proc (ss-serve))
	(delete-process proc))
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

;;; Request:
(defun ss-request-host ()
  "returns the host name of the request"
  (cdr (assoc 'host ss-request)))

(defun ss-request-method ()
  "returns the method as a keyword i.e. :get"
  (cdr (assoc 'method ss-request)))

(defun ss-request-uri ()
  "returns the path of the uri following the"
  (cdr (assoc 'uri ss-request)))

(defun ss-serve ()
  "This function should take a route, and using the functions in
the `ss-dispatch-table' it will try to handle the
request.  This function will then wrap any responce as
appropriate and return it as a string to be given directly to the
process."
  (let (handler)
    ;; set the request object
    (dolist (dispatcher ss-dispatch-table)
      (when (null handler)
	(setf handler (funcall dispatcher))))
    (message (format "%S" handler))
    (if handler
	(concat "HTTP/1.1 200 OK
Connection: close
Status: 200 OK
Server: Emacs SimpleServer
Content-Type: text/html; charset=utf-8\n\n"
		(funcall handler)
;;;             ;; Error handling
;;; 		(condition-case err
;;; 		    (funcall handler)
;;; 		  "HTTP/1.1 500 Server Error
;;; Connection: close
;;; Status: 500 Server Error
;;; Server: Emacs SimpleServer\n\n")
		)
		  "HTTP/1.1 404 Not Found
Connection: close
Status: 404 Not Found
Server: Emacs SimpleServer\n\n")))

;;; Specifying Content
;;
;;; All Possible Headers
;; HTTP/1.1 200 OK
;; Connection: close
;; Date: Wed, 19 Nov 2008 01:40:26 GMT
;; Set-Cookie: _emacs_session=076da85608ca541e04dbe1e276505cd9; path=/
;; Status: 200 OK
;; X-Runtime: 0.02498
;; ETag: \"662c5a2975130ea13d1c2acae196ae72\"
;; Cache-Control: private, max-age=0, must-revalidate
;; Server: Mongrel 1.1.5
;; Content-Type: text/html; charset=utf-8
;; Content-Length: 78435
(defvar ss-default-page
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
	\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
<head>
<title>Simple Server is up and running</title>
</head>
<body>
<h2>Simple Server</h2>
<p>
<b>Simple Server</b> is a VERY simple web-server written entirely<br>
in emacs lisp.  This is made possible thanks to the addition of<br>
server-sockets to emacs via the `make-network-process' command.<br>
</p>
<p>
I would not recommend actually using this in anything even<br>
resembling a production environment.
</p>
<p>
Now using a dispatch table.
</p>
-- schulte
</body>
</html>\n"
"This is the default page for the Emacs simple-server web server.")

(provide 'simple-server)
;;; simple-server.el ends here