;;; init-ai.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - AI-related related settings

;;; Code:

(defun is-work-laptop ()
  nil)

(use-package agent-shell
  :ensure t
  :config
  (setq agent-shell-mcp-servers
        `(((name . "context7")
           (type . "http")
           (headers . (((name . "CONTEXT7_API_KEY")
                        (value . ,(auth-source-pick-first-password :machine "mcp.context7.com")))))
           (url . "https://mcp.context7.com/mcp"))))

  (if (is-work-laptop)
      (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication
         :api-key (lambda ()
                    (auth-source-pick-first-password :machine "api.openai.com"))))
    (setq agent-shell-mistral-authentication
          (agent-shell-mistral-make-authentication
           :api-key (lambda ()
                      (auth-source-pick-first-password :machine "api-key.mistral.com")))))

  (setq agent-shell-preferred-agent-config
        (if (is-work-laptop)
            (agent-shell-openai-make-codex-config)
          (agent-shell-mistral-make-config)))
  )

(provide 'init-ai)

;; init-ai.el ends here
