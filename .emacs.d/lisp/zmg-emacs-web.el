;;; zmg-web.el --- Web-related configurations -*- lexical-binding: t -*-

;; Copyright (c) 2019-2022  Protesilaos Stavrou <info@protesilaos.com>

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See my dotfiles: <https://git.sr.ht/~protesilaos/dotfiles>

;;; Code:

;;; -----------------------------
;;; IRC
;;; ------------------------------

(use-package rcirc
  :config
  (setq rcirc-server-alist
        '(("irc.libera.chat"
           :channels ("#openbsd" "#lisp"))))
  (setq rcirc-default-nick "zmyrgel")
  (setq rcirc-default-user-name "zmyrgel")
  (setq rcirc-default-full-name "Curious Minds Want To Know")

  (let ((nickserv-pass (secrets-get-secret "default" "libera-pass")))
    (when nickserv-pass
      (setq rcirc-authinfo
            `(("libera" nickserv "zmyrgel" ,nickserv-pass)))))

  (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
  (setq rcirc-time-format "%Y-%m-%d %H:%M ")
  :hook ((rcirc-mode-hook . rcirc-track-minor-mode)))

(use-package erc
  :hook ((erc-mode-hook . erc-services-mode)
         (erc-mode-hook . erc-autojoin-mode)
         (erc-mode-hook . erc-match-mode)
         (erc-mode-hook . erc-track-mode)
         (erc-mode-hook . erc-fill-mode)
         (erc-mode-hook . erc-ring-mode)
         (erc-mode-hook . erc-netsplit-mode)
         (erc-mode-hook . erc-timestamp-mode)
         (erc-mode-hook . erc-spelling-mode)
         (erc-mode-hook . erc-notify-mode)
         (erc-mode-hook . erc-pcomplete-mode)
         (erc-mode-hook . erc-log-mode)
         (erc-insert-post-hook . erc-save-buffer-in-logs)
         (erc-insert-post-hook . erc-truncate-buffer))
  :config
  (setq erc-modules (append erc-modules '(services notify spelling log)))
  (erc-update-modules)

  (setq erc-prompt-for-password nil)
  (setq erc-kill-buffer-on-part t)
  (setq erc-kill-queries-on-quit nil)
  (setq erc-kill-server-buffer-on-quit t)
  (setq erc-auto-query 'window-noselect)
  (setq erc-keywords '("zmyrgel" "tmy"))

  (setq erc-track-enable-keybindings t)
  (setq erc-track-remove-disconnected-buffers t)
  (setq erc-track-exclude-server-buffer t)
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (setq erc-timestamp-format "[%R-%m/%d]")
  (setq erc-hide-timestamps nil)

  (pcomplete-erc-setup)

  (setq erc-pcomplete-order-nickname-completions t)
  (setq erc-log-channels-directory (locate-user-emacs-file "erc-logs"))
  (setq erc-log-insert-log-on-open nil)
  (setq erc-log-file-coding-system 'utf-8-unix)
  (setq erc-save-buffer-on-part t)
  (setq erc-max-buffer-size 20000)
  (setq erc-truncate-buffer-on-save t)
  (defvar erc-insert-post-hook nil))


;;; ------------------------------
;;; Web Browsing settings
;;; ------------------------------

(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory "~/.emacs.d/elfeed/")
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@4-months-ago +unread")
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)

  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "http://planet.emacsen.org/atom.xml"
          "https://news.ycombinator.com/rss"
          "http://www.tedunangst.com/flak/rss"
          "https://undeadly.org/cgi?action=rss"
          "https://www.phoronix.com/rss.php"
          "http://planetsysadmin.com/atom.xml"
          ("http://oremacs.com/atom.xml" emacs)
          ("http://emacsblog.org/feed/" emacs)
          ("http://endlessparentheses.com/atom.xml" emacs)
          ("https://www.masteringemacs.org/feed" emacs)
          "https://scripter.co/posts/atom.xml"
          ("https://oneofus.la/have-emacs-will-hack/feed.xml" emacs)
          ("https://updates.orgmode.org/feed/changes" emacs org)
          ("https://www.reddit.com/r/emacs.rss" emacs reddit)
          ("https://www.reddit.com/r/orgmode.rss" reddit emacs org)
          ("https://xkcd.com/atom.xml" xkcd)
          ("https://planet.lisp.org/rss20.xml" lisp)
          "https://lobste.rs/t/emacs.lisp.security.ask.ai.openbsd.programming.rss")))

;; | M-s M-w | eww-search-words       |
(use-package eww
  :commands (eww
             eww-browse-url
             eww-search-words
             eww-open-in-new-buffer
             eww-open-file)
  :config
  (setq eww-restore-desktop nil)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format "%u")
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory (expand-file-name "Downloads" "~"))
  (setq eww-suggest-uris
        '(eww-links-at-point thing-at-point-url-at-point))
  (setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks"))
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio/\\|application/ogg\\|pdf\\)")
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")
  :bind (:map eww-mode-map
              ("n" . next-line)
              ("p" . previous-line)
              ("f" . forward-char)
              ("b" . backward-char)
              ("B" . eww-back-url)
              ("N" . eww-next-url)
              ("P" . eww-previous-url)))

(use-package browse-url
  :after eww
  :config
  (setq browse-url-new-window-flag nil)
  (setq browse-url-firefox-new-window-is-tab t)
  (setq browse-url-browser-function 'eww-browse-url))

(use-package restclient
  :ensure t
  :defer t
  :mode (("\\.http\\'" . restclient-mode))
  ;;TODO: change to only apply json formatting when the content-type is
  ;;application/json
  :bind (:map restclient-mode-map
              ("C-c C-f" . json-mode-beautify)))

(provide 'zmg-emacs-web)
