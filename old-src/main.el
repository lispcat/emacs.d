;; [[file:../README.org::*debugging][debugging:1]]
;; (advice-add 'switch-to-buffer :before (lambda (arg &optional a b) (message "DEBUG: switching to buffer: %s" arg)))
;; (debug-on-entry 'switch-to-buffer)
;; debugging:1 ends here

;; [[file:../README.org::*ending][ending:1]]
(provide 'main)
;; ending:1 ends here
