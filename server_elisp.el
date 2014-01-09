;; requires Emacs 24 or later
;; run with "emacs -Q -l server_elisp.el"
(make-network-process
 :name "echo-server"
 :service 5000
 :filter (lambda (process string)
           (process-send-string process string)
           (delete-process process))
 :server t
 :nowait t
 :family (quote ipv4))
