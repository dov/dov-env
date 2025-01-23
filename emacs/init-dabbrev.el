;; other misc setup
(setq dabbrev-case-fold-search nil) ; make dabbrev case sensitive

(global-set-key [(control ?.)] 'dabbrev-expand)
(global-set-key [(alt /)] 'dabbrev-expand)
(global-set-key [(control /)] 'dabbrev-expand)


;; Define abbreviations
(define-abbrev-table 'global-abbrev-table '(
    ;; c-mode                                            
    ("vropt" "vsg::ref_ptr<vsg::Options> options")
    ("vrx" "vsg::ref_ptr<vsg::> ")
))

(provide 'init-dabbrev)
