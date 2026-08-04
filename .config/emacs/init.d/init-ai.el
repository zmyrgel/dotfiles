;;; init-ai.el --- Init AI File -*- lexical-binding: t -*-

;; Copyright (c) 2026  Timo Myyrä <timo.myyra@bittivirhe.fi>

;; Author: Timo Myyrä <timo.myyra@bittivirhe.fi>
;; URL: https://github.com/zmyrgel/dotfiles
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; - AI-related related settings

;;; Code:

;;; agent-shell
(ensure-packages-present 'agent-shell)

(keymap-global-set "C-c ." 'agent-shell)

(with-eval-after-load 'agent-shell
  (when work-laptop-p
    (setopt agent-shell-openai-authentication
            (agent-shell-openai-make-authentication
             :api-key (lambda ()
                        (password-lookup :host "api.openai.com"))))
    (setopt agent-shell-preferred-agent-config
            (agent-shell-openai-make-codex-config))
    (setopt agent-shell-clipboard-image-handlers
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

  (unless work-laptop-p
    (setopt agent-shell-mistral-authentication (agent-shell-mistral-make-authentication
                                                :api-key (lambda ()
                                                           (auth-source-pick-first-password :host "api-key.mistral.com"))))
    (setopt agent-shell-preferred-agent-config (agent-shell-mistral-make-config)))

  ;; Setup MCP servers to share between agents
  (setopt agent-shell-mcp-servers
          `(((name . "context7")
             (type . "http")
             (headers . (((name . "CONTEXT7_API_KEY")
                          (value . ,(password-lookup :host "mcp.context7.com")))))
             (url . "https://mcp.context7.com/mcp")))))

(provide 'init-ai)

;;; init-ai.el ends here
