(add-to-list 'tramp-connection-properties
             (list (regexp-quote "/ssh:gropi:")
                   "login-args"
                   '("-q" ("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("%h"))))

(provide 'init-tramp)
