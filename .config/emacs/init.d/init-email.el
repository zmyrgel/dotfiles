;;; init-email.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - setup email related settings

;;; Code:

(setopt user-full-name "Timo Myyrä")

;; smtpmail
(unless (is-work-laptop-p)
  (setopt smtpmail-default-smtp-server "smtp.fastmail.com")
  (setopt smtpmail-smtp-server         "smtp.fastmail.com")
  (setopt smtpmail-local-domain        "bittivirhe.fi")
  (setopt smtpmail-smtp-service        465)
  (setopt smtpmail-stream-type         'ssl))

(setopt message-send-mail-function 'smtpmail-send-it)
(setopt send-mail-function 'smtpmail-send-it)

;; mml
(setopt mml-attach-file-at-the-end t)

;; message
(with-eval-after-load 'gnus
  (setopt mail-user-agent 'gnus-user-agent)
  (setopt message-mail-user-agent nil)
  (setopt compose-mail-user-agent-warnings nil)
  (setopt message-citation-line-function 'message-insert-formatted-citation-line)
  (setopt message-confirm-send nil)
  (setopt message-kill-buffer-on-exit t)
  (setopt message-wide-reply-confirm-recipients t)
  (add-hook 'message-setup-hook 'message-sort-headers)

  (add-hook 'gnus-started-hook
            (lambda ()
              (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))))

  (setopt gnus-gcc-mark-as-read t)
  (setopt gnus-always-read-dribble-file t)
  (setopt mm-inline-large-images 'resize)
  (setopt mm-discouraged-alternatives '("text/html" "text/richtext"))
  (setopt mm-text-html-renderer 'shr)
  (setopt gnus-select-method '(nntp "news.gmane.io"))
  (setopt gnus-secondary-select-methods
          '((nnimap "home"
                    (nnimap-address "imap.fastmail.com")
                    (nnir-search-engine imap)
                    (nnimap-stream tls)
                    (nnmail-expiry-target "nnimap+home:Trash"))))

  (setopt gnus-posting-styles
          '((".*"
             (address "Timo Myyrä <timo.myyra@bittivirhe.fi>")
             (gcc "nnimap+home:Sent"))))

  (setopt gnus-visible-headers
          '("^From:" "^Subject:" "^To:"
            "^Cc:" "^Newsgroups:" "^Date:"
            "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
            "^X-Mailer:"))
  (setopt gnus-sorted-header-list gnus-visible-headers)

  (setopt gnus-auto-expirable-newsgroups
          "nnimap\\+home:\\(ABCL\\|CHICKEN\\|OpenBSD\\|Postgresql-general\\|SBCL\\)")

  ;; gnus-async
  (setopt gnus-asynchronous t)
  (setopt gnus-use-article-prefetch t)

  ;; nnmail
  (setopt nnmail-expiry-wait 7)

  ;; gnus-agent
  (setopt gnus-agent-expire-days 7)

  ;; do not load images by default
  (setopt gnus-inhibit-images t)

  ;; gnus-dired
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))

(provide 'init-email)

;; init-email.el ends here
