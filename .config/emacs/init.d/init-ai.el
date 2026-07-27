;;; init-ai.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - AI-related related settings

;;; Code:

(use-package agent-shell
  :ensure t
  :bind (("C-c ." . agent-shell))
  :config
  (when (is-work-laptop-p)
    (setq agent-shell-openai-authentication
          (agent-shell-openai-make-authentication
           :api-key (lambda ()
                      (password-lookup :host "api.openai.com"))))
    (setq agent-shell-preferred-agent-config
          (agent-shell-openai-make-codex-config))
    (setq agent-shell-clipboard-image-handlers
          (list
           (list (cons :command "xclip")
                 (cons :save (lambda (file-path)
                               (when-let* ((targets (and (eq (window-system) 'x)
                                                         (gui-get-selection 'CLIPBOARD 'TARGETS)))
                                           ((vectorp targets))
                                           ((not (seq-contains-p targets 'image/png))))
                                 (error "No image/png in clipboard"))
                               (with-temp-buffer
                                 (set-buffer-multibyte nil)
                                 (let ((exit-code (call-process "xclip" nil t nil
                                                                "-selection" "clipboard"
                                                                "-t" "image/png" "-o")))
                                   (unless (zerop exit-code)
                                     (error "Command xclip failed with exit code %d" exit-code))
                                   (write-region (point-min) (point-max) file-path nil 'silent)))))))))

  (unless (is-work-laptop-p)
    (setq agent-shell-mistral-authentication (agent-shell-mistral-make-authentication
                                              :api-key (lambda ()
                                                         (auth-source-pick-first-password :host "api-key.mistral.com"))))
    (setq agent-shell-preferred-agent-config (agent-shell-mistral-make-config)))

  ;; Setup MCP servers to share between agents
  (setq agent-shell-mcp-servers
        `(((name . "context7")
           (type . "http")
           (headers . (((name . "CONTEXT7_API_KEY")
                        (value . ,(password-lookup :host "mcp.context7.com")))))
           (url . "https://mcp.context7.com/mcp"))))
  )

(provide 'init-ai)

;; init-ai.el ends here
