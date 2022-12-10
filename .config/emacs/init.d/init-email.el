;;; init-email.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - setup email related settings

;;; Code:

(setq user-mail-address "timo.myyra@bittivirhe.fi")
(setq user-full-name "Timo Myyrä")

;; smtpmail
(setq smtpmail-default-smtp-server "smtp.fastmail.com")
(setq smtpmail-smtp-server         "smtp.fastmail.com")
(setq smtpmail-local-domain        "bittivirhe.fi")
(setq smtpmail-smtp-service        465)
(setq smtpmail-stream-type         'ssl)

(setq message-send-mail-function 'smtpmail-send-it)
(setq send-mail-function 'smtpmail-send-it)

;; message
(with-eval-after-load 'gnus
  (setq mail-user-agent 'message-user-agent)
  (setq message-mail-user-agent nil)    ; default is `gnus'
  (setq compose-mail-user-agent-warnings nil)
  (setq message-citation-line-format "%f [%Y-%m-%d, %R %z]:\n")
  (setq message-citation-line-function
        'message-insert-formatted-citation-line)
  (setq message-confirm-send nil)
  (setq message-kill-buffer-on-exit t)
  (setq message-wide-reply-confirm-recipients t)
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64)) ;; FIXME: undefined
  (add-hook 'message-setup-hook 'message-sort-headers)

  (setq gnus-extra-headers
        '(To Newsgroups X-GM-LABELS))

  ;; gnus
  (setq gnus-treat-hide-citation t)
  (setq gnus-gcc-mark-as-read t)
  (setq gnus-cited-lines-visible '(0 . 5))
  (setq gnus-always-read-dribble-file t)
  (setq mm-inline-large-images 'resize)
  (setq mm-discouraged-alternatives '("text/html" "text/richtext"))
  (setq mm-text-html-renderer 'shr)
  (setq gnus-select-method '(nntp "news.gmane.io"))
  (setq gnus-secondary-select-methods
        '((nnimap "work-gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port "993")
                  (nnir-search-engine imap)
                  (nnimap-stream ssl))
          (nnimap "fastmail"
                  (nnimap-address "imap.fastmail.com")
                  (nnir-search-engine imap)
                  (nnimap-stream tls))))

  (setq gnus-article-mode-line-format "%G %S %m")
  (setq gnus-visible-headers
        '("^From:" "^Subject:" "^To:" "^Cc:" "^Newsgroups:" "^Date:"
          "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
          "^X-Mailer:"))
  (setq gnus-sorted-header-list gnus-visible-headers)

  ;; gnus-async
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15)

  ;; nnmail
  (setq nnmail-expiry-wait 30)

  ;; gnus-agent
  (setq gnus-agent-expire-days 30)

  ;; gnus-dired
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))

(global-set-key (kbd "C-z m") 'gnus)

(provide 'init-email)

;; init-email.el ends here
