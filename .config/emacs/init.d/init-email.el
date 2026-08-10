;;; init-email.el --- Email related configuration -*- lexical-binding: t -*-

;; Copyright (c) 2022-2026 Timo Myyrä <timo.myyra@bittivirhe.fi>

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

;;; - setup email related settings

;;; Code:

;;; emacs
(setopt user-full-name "Timo Myyrä")

;;; simple
(setopt mail-user-agent 'gnus-user-agent)
(setopt compose-mail-user-agent-warnings nil)

;;; message
(setopt message-send-mail-function 'smtpmail-send-it)
(setopt message-mail-user-agent nil)
(setopt message-citation-line-function 'message-insert-formatted-citation-line)
(setopt message-confirm-send nil)
(setopt message-kill-buffer-on-exit t)
(setopt message-wide-reply-confirm-recipients t)
(add-hook 'message-setup-hook 'message-sort-headers)

;;; smtpmail
(unless work-laptop-p
  (setopt smtpmail-default-smtp-server "smtp.fastmail.com")
  (setopt smtpmail-smtp-server         "smtp.fastmail.com")
  (setopt smtpmail-local-domain        "bittivirhe.fi")
  (setopt smtpmail-smtp-service        465)
  (setopt smtpmail-stream-type         'ssl))

;;; sendmail
(setopt send-mail-function 'smtpmail-send-it)

;;; mml
(setopt mml-attach-file-at-the-end t)

;;; gnus
(with-eval-after-load 'gnus
  (add-to-list
   'gnus-newsgroup-variables
   '(mm-discouraged-alternatives
     . '("text/html" "image/.*")))

  ;; Display html in rss groups
  (add-to-list
   'gnus-parameters
   '("\\`nnrss:" (mm-discouraged-alternatives nil)))

  (add-hook 'gnus-started-hook
            (lambda ()
              (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64)))))

  ;;; gnus-msg
(setopt gnus-gcc-mark-as-read t)

  ;;; gnus-start
(setopt gnus-always-read-dribble-file t)

  ;;; mm-decode
(setopt mm-inline-large-images 'resize)
(setopt mm-discouraged-alternatives '("text/html" "text/richtext"))
(setopt mm-text-html-renderer 'shr)

  ;;; gnus
(setopt gnus-select-method '(nntp "news.gmane.io"))
(setopt gnus-secondary-select-methods
        '((nnimap "home"
                  (nnimap-address "imap.fastmail.com")
                  (nnir-search-engine imap)
                  (nnimap-stream tls)
                  (nnimap-expunge 'on-exit)
                  (nnmail-expiry-target "nnimap+home:Trash")
                  (nnimap-streaming t))))

(setopt gnus-auto-expirable-newsgroups
        "nnimap\\+home:\\(ABCL\\|CHICKEN\\|OpenBSD\\|Postgresql-general\\|SBCL\\)")

;; gnus-msg
(setopt gnus-posting-styles
        '((".*"
           (address "Timo Myyrä <timo.myyra@bittivirhe.fi>")
           (gcc "nnimap+home:Sent"))))

;; gnus-art
(setopt gnus-visible-headers
        '("^From:" "^Subject:" "^To:"
          "^Cc:" "^Newsgroups:" "^Date:"
          "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
          "^X-Mailer:"))
(setopt gnus-sorted-header-list gnus-visible-headers)
(setopt gnus-inhibit-images t)

;;; gnus-async
(setopt gnus-asynchronous t)
(setopt gnus-use-article-prefetch t)

;;; nnmail
(setopt nnmail-expiry-wait 7)

;;; gnus-agent
(setopt gnus-agent-expire-days 7)

;;; gnus-dired
;; {C-c C-m C-a} gnus-dired-attach
;; {C-c C-m C-l} gnus-dired-find-file-mailcap
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; Cloud sync:: to do, epg needs some work
;; {~ RET} / {~ ~}

;; https://josefsson.org/inline-openpgp-considered-harmful.html

(provide 'init-email)

;;; init-email.el ends here
