;;; simple-server.el --- a SIMPLE web-server run by emacs

;; Copyright (C) 2008 Eric Schulte

;; Author: Eric Schulte

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Attribution:

;; This borrows heavily from the EmacsEchoServer on the Emacs Wiki at
;; http://www.emacswiki.org/emacs/EmacsEchoServer, and from httpd.el
;; by Eric Marsden and John Wiegley

;;; Commentary:

;; `simple-server' implements a very simple http server which will
;; hopefully be able to hand off GET requests to functions specified
;; in the `simple-server-dispatch-table'.
;;
;; Maybe one day it will even handle post requests.

;;; Code:
;; require statements

;;; Customization:
(defvar simple-server-clients '() 
  "alist where KEY is a client process and VALUE is the string")
(defvar simple-server-name "web-server")
(defvar simple-server-buffer "*web-server*")
(defvar simple-server-log "*simple-server-log*")
(defvar simple-server-dispatch-table
  '((lambda (route) (if (string-equal "/" route) simple-server-default-page nil)))
  "List of functions which accept the route of the request, and
then return a page.  If a function returns nil, then the next
function on the list is called.  These functions are tried in
order.  The last function on the list should render the default
page.")

;;; Control Functions:
(defun simple-server-start ()
  "Starts a simple emacs webserver"
  (interactive)
  (unless (process-status simple-server-name)
    (make-network-process :name simple-server-name
			  :buffer simple-server-buffer
			  :family 'ipv4
			  :service 3000
			  :sentinel 'simple-server-sentinel
			  :filter 'simple-server-filter
			  :server 't) 
    (setq echo-server-clients '())))

(defun simple-server-stop ()
  "stop an emacs simple server"
  (interactive)
  (while  simple-server-clients
    (delete-process (car (car simple-server-clients)))
    (setq simple-server-clients (cdr simple-server-clients)))
  (delete-process simple-server-name))

(defun simple-server-toggle ()
  "turn the simple-server on or off"
  (interactive)
  (if (process-status simple-server-name) (simple-server-stop) (simple-server-start)))

(defun simple-server-restart ()
  "restart the emacs simple server"
  (interactive) (simple-server-stop) (simple-server-start))

;;; Implementation Fuctions:
(defun simple-server-filter (proc string)   
  (let ((pending (assoc proc simple-server-clients))
        message index route)
    ;;create entry if required
    (unless pending
      (setq simple-server-clients (cons (cons proc "") simple-server-clients))
      (setq pending  (assoc proc simple-server-clients)))
    (setq message (concat (cdr pending) string))
    (while (setq index (string-match "\n" message)) ;; this is the main server loop
      ;; collect the request
      (setq index (1+ index))
      (simple-server-log  (substring message 0 index) "out" proc)
      ;; find the route
      (when (string-match "GET \\([^ ]+\\) HTTP" message)
	(setf route (match-string 1 message)) (simple-server-log route "route"))
      ;; when the request ends reply
      (when (string-equal "\r\n" message)
	(process-send-string
	 proc
	 (simple-server-handle-request route))
	(delete-process proc))
      (setq message (substring message index)))
    (setcdr pending message)))

(defun simple-server-sentinel (proc msg)
  (delq proc simple-server-clients)
  (simple-server-log (format "client %s has quit" proc) "quit"))

(defun simple-server-log (string in-or-out &optional client)
  "If a *simple-server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer simple-server-log)
      (with-current-buffer simple-server-log
	(goto-char (point-max))
	(insert in-or-out
		(if client (format " %s:" client) " ")
		string)
	(or (bolp) (newline)))))

(defun simple-server-handle-request (route)
  "This function should take a route, and using the functions in
the `simple-server-dispatch-table' it will try to handle the
request.  This function will then wrap any responce as
appropriate and return it as a string to be given directly to the
process."
  (message route)
  (let (responce)
    (dolist (func simple-server-dispatch-table)
      (when (null responce)
	(setf responce (funcall func route))))
    (if responce
	(concat "HTTP/1.1 200 OK
Connection: close
Status: 200 OK
Server: Emacs SimpleServer
Content-Type: text/html; charset=utf-8\n\n" responce)
      "HTTP/1.1 404 Not found")))

;;; Specifying Content

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
(defvar simple-server-default-page
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