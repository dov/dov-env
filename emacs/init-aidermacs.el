(setq aidermacs-program "~/venv/aider/bin/aider")

(use-package aidermacs
  :ensure t
  :config
  (let ((copilot-token
         ;; Read the token from the specified file path
         (with-temp-buffer
           (when (file-readable-p "~/.config/copilot-chat/github-token")
             (insert-file-contents "~/.config/copilot-chat/github-token")
             ;; Trim any surrounding whitespace/newlines from the token
             (string-trim (buffer-string)))))
        )
    ;; Only set the environment variables if a token was successfully read
    (when copilot-token
      (setenv "OPENAI_API_KEY" copilot-token)
      (setenv "OPENAI_API_BASE" "https://api.githubcopilot.com"))
    )
  )

(global-set-key "\C-ca" 'aidermacs-transient-menu)

(provide 'init-aidermacs)
