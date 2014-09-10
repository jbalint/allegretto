; unique server name - Lua client uses named server for communication
(setq server-name "allegretto")

; *R* should be started before Allegretto
(defun allegretto-start ()
  "Start the Allegretto process"
  (interactive)
  (async-shell-command "love ."))

