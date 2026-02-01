;;; init-email.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - setup email related settings

;;; Code:

(setq user-full-name "Timo Myyrä")

;; smtpmail
(setq smtpmail-default-smtp-server "smtp.fastmail.com")
(setq smtpmail-smtp-server         "smtp.fastmail.com")
(setq smtpmail-local-domain        "bittivirhe.fi")
(setq smtpmail-smtp-service        465)
(setq smtpmail-stream-type         'ssl)

(setq message-send-mail-function 'smtpmail-send-it)
(setq send-mail-function 'smtpmail-send-it)

;; mml
(setq mml-attach-file-at-the-end t)

;; message
(with-eval-after-load 'gnus
  (setq mail-user-agent 'gnus-user-agent)
  (setq message-mail-user-agent nil)
  (setq compose-mail-user-agent-warnings nil)
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-confirm-send nil)
  (setq message-kill-buffer-on-exit t)
  (setq message-wide-reply-confirm-recipients t)
  (add-hook 'message-setup-hook 'message-sort-headers)

  (add-hook 'gnus-started-hook
            (lambda ()
              (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))))

  (setq gnus-gcc-mark-as-read t)
  (setq gnus-always-read-dribble-file t)
  (setq mm-inline-large-images 'resize)
  (setq mm-discouraged-alternatives '("text/html" "text/richtext"))
  (setq mm-text-html-renderer 'shr)
  (setq gnus-select-method '(nntp "news.gmane.io"))
  (setq gnus-secondary-select-methods
        '((nnimap "home"
                  (nnimap-address "imap.fastmail.com")
                  (nnir-search-engine imap)
                  (nnimap-stream tls)
                  (nnmail-expiry-target "nnimap+home:Trash"))))

  (setq gnus-posting-styles
        '((".*"
           (address "Timo Myyrä <timo.myyra@bittivirhe.fi>")
           (gcc "nnimap+home:Sent"))))

  (setq gnus-visible-headers
        '("^From:" "^Subject:" "^To:"
          "^Cc:" "^Newsgroups:" "^Date:"
          "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
          "^X-Mailer:"))
  (setq gnus-sorted-header-list gnus-visible-headers)

  (setq gnus-auto-expirable-newsgroups
        "nnimap\\+home:\\(ABCL\\|CHICKEN\\|OpenBSD\\|Postgresql-general\\|SBCL\\)")

  ;; gnus-async
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch t)

  ;; nnmail
  (setq nnmail-expiry-wait 7)

  ;; gnus-agent
  (setq gnus-agent-expire-days 7)

  ;; do not load images by default
  (setq gnus-inhibit-images t)

  ;; gnus-dired
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))

(require 'mm-url)
(defadvice mm-url-insert (after DE-convert-atom-to-rss () )
  "Converts atom to RSS by calling xsltproc."
  (unless (executable-find "xsltproc")
    (display-warning 'init-email
                     (format "Missing `%s' file, remember to download it" atom2rss-file) :warning))
  (when (re-search-forward "xmlns=\"http://www.w3.org/.*/Atom\""
			   nil t)
    (let ((atom2rss-file (locate-user-emacs-file "atom2rss.xsl")))
      (when (not (file-exists-p atom2rss-file))
          (display-warning 'init-email
                           (format "Missing `%s' file, downloading it" atom2rss-file) :warning)
          (require 'url)
          (url-copy-file "https://atom.geekhood.net/atom2rss.xsl" atom2rss-file))
        (goto-char (point-min))
        (message "Converting Atom to RSS... ")
        (call-process-region (point-min) (point-max)
			     "xsltproc"
			     t t nil
			     atom2rss-file "-")
        (goto-char (point-min))
        (message "Converting Atom to RSS... done"))))

(global-set-key (kbd "C-z m") 'gnus)

(provide 'init-email)

;; init-email.el ends here
