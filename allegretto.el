; unique server name - Lua client uses named server for communication
(setq server-name "allegretto")

; *R* should be started before Allegretto
(defun allegretto-start ()
  "Start the Allegretto process"
  (interactive)
  (async-shell-command "love ."))

;; query the DB
(defun tos-get-history-list ()
  "Retrieve the list of available (TOS) trade histories"
  (let ((mysql-command
		 (concat "mysql "
				 "-u root -P 3319 -h 127.0.0.1 "
				 "--batch --raw --skip-column-names "
				 "thinkorswim "
				 "-e \"select * from capture_info\"")))
	(shell-command-to-string mysql-command)))

(mapcar (lambda (line)
		  (let* ((vals (split-string line "\t"))
				 (uuid (car vals)))
			(widget-create 'link
						   :uuid uuid ; save the uuid for access in :notify
						   :notify (lambda (widget &rest _x)
									 (message (widget-get widget :uuid)))
						   (concat (format " %4s " (nth 1 vals))
								   (car (split-string (nth 3 vals))) " / "
								   (car (split-string (nth 4 vals))) " "))
			(insert "\n")))
		(split-string (tos-get-history-list) "\n" t))
