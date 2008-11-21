;;; emacs-web.el --- a pure emacs webserver
;;
;; tweaking of EmacsEchoServer from
;; http://www.emacswiki.org/emacs/EmacsEchoServer
;;
;;
(defvar echo-server-port 8080
  "port of the echo server")

(defvar echo-server-clients '() 
  "alist where KEY is a client process and VALUE is the string")

(defun echo-server-start nil
  "starts an emacs echo server"
  (interactive)
  (unless (process-status "echo-server")
    (make-network-process :name "echo-server"
			  :buffer "*echo-server*"
			  :family 'ipv4
			  :service echo-server-port
			  :sentinel 'echo-server-sentinel
			  :filter 'echo-server-filter
			  :server 't) 
    (setq echo-server-clients '())))

(defun echo-server-stop nil
  "stop an emacs echo server"
  (interactive)
  (while  echo-server-clients
    (delete-process (car (car echo-server-clients)))
    (setq echo-server-clients (cdr echo-server-clients)))
  (delete-process "echo-server"))

(defun echo-server-filter (proc string)   
  (let ((pending (assoc proc echo-server-clients))
        message index)
    ;;create entry if required
    (unless pending
      (setq echo-server-clients (cons (cons proc "") echo-server-clients))
      (setq pending  (assoc proc echo-server-clients)))
    (setq message (concat (cdr pending) string))
    (while (setq index (string-match "\n" message))
      (setq index (1+ index))
      (process-send-string proc (substring message 0 index))
      (echo-server-log  (substring message 0 index) proc)
      (setq message (substring message index)))
    (setcdr pending message)))

(defun echo-server-sentinel (proc msg)
  (delq proc echo-server-clients)
  (echo-server-log (format "client %s has quit" proc)))

;;from server.el
(defun echo-server-log (string &optional client)
  "If a *echo-server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*echo-server*")
      (with-current-buffer "*echo-server*"
	(goto-char (point-max))
	(insert (current-time-string)
		(if client (format " %s:" client) " ")
		string)
	(or (bolp) (newline)))))

;;; emacs-web.el ends here