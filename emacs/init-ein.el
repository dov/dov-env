(setq load-path (append
                 (list
                  (concat emacs-git "ein/lisp")
                  )
                 load-path))
		  
(require 'ein)
(require 'ein-notebook)
(require 'ein-jupyter)
(require 'ein-subpackages)

(add-hook 'ein:notebook-multilang-mode-hook
          (lambda()
            (define-key ein:notebook-mode-map [(control up)] 'scroll-up-line)
            (define-key ein:notebook-mode-map [(control down)] 'scroll-down-line)
            (define-key ein:notebook-mode-map [(return)] 'newline-and-indent)
            (define-key ein:notebook-mode-map [(control c) ?t] 'ein:worksheet-change-cell-type)
            (setq ein:worksheet-enable-undo t)))

(add-hook 'ein:notebook-mode-hook
          (lambda()
            (define-key ein:notebook-mode-map [(control up)] 'scroll-up-line)
            (define-key ein:notebook-mode-map [(control down)] 'scroll-down-line)
            (define-key ein:notebook-mode-map [(return)] 'newline-and-indent)
            (define-key ein:notebook-mode-map [(control c) ?t] 'ein:worksheet-change-cell-type)
            (setq ein:worksheet-enable-undo t)))

(provide 'init-ein)
