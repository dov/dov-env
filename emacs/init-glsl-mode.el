(setq load-path (append
                 (list
                  (concat emacs-git "packages/glsl-mode-20250324.1304/")
                  )
                 load-path))
		  
(require 'init-flycheck)
(require 'glsl-mode)

;; Associate GLSL shader file extensions with glsl-mode
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;; Enable flycheck-mode automatically in glsl-mode buffers
(add-hook 'glsl-mode-hook #'flycheck-mode)

;; Ensure glslangValidator is in your PATH and add the Flycheck checker for glsl-mode
(with-eval-after-load 'flycheck
  (flycheck-define-checker glsl-glslangValidator
    "A GLSL syntax checker using glslangValidator."
    :command ("glslangValidator" "-V" source)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": error:" (message) line-end)
     (warning line-start (file-name) ":" line ":" column ": warning:" (message) line-end))
    :modes glsl-mode)
  (add-to-list 'flycheck-checkers 'glsl-glslangValidator))

;; Optionally, select the checker automatically in glsl-mode
(add-hook 'glsl-mode-hook
          (lambda ()
            (when (executable-find "glslangValidator")
              (flycheck-select-checker 'glsl-glslangValidator))))


(provide 'init-glsl-mode)
