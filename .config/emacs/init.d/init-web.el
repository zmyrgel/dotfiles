;;; init-web.el --- Web browsing and Internet service uses -*- lexical-binding: t -*-

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

;;; Code:

(setq telnet-program "nc")

;;; rcirc
(with-eval-after-load 'rcirc
  (setopt rcirc-server-alist
          '(("irc.libera.chat"
             :channels ("#openbsd" "#lisp" "#emacs" "#gameoftrees")
             :port 6697
             :encryption tls)))
  (setopt rcirc-default-nick "zmyrgel")
  (setopt rcirc-default-user-name "zmyrgel")
  (setopt rcirc-default-full-name "Curious Minds Want To Know")
  (setopt rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

  ;; {C-c C-SPC} to switch to urgent buffer
  (defun my/rcirc-setup ()
    (setq-local scroll-conservatively 8192))

  (add-hook 'rcirc-mode-hook #'my/rcirc-setup)
  (add-hook 'rcirc-mode-hook #'flyspell-mode)
  (add-hook 'rcirc-mode-hook #'rcirc-track-minor-mode)
  (add-hook 'rcirc-mode-hook #'rcirc-omit-mode)

  (setopt rcirc-authinfo
          '(("irc.libera.chat" nickserv "zmyrgel" :auth-source)))

  (setopt rcirc-time-format "%Y-%m-%d %H:%M ")
  (setopt rcirc-log-time-format "%Y-%m-%d %H:%M "))

;;;  erc
(with-eval-after-load 'erc
  (add-hook 'erc-mode-hook 'erc-services-mode)
  (add-hook 'erc-mode-hook 'erc-autojoin-mode)
  (add-hook 'erc-mode-hook 'erc-match-mode)
  (add-hook 'erc-mode-hook 'erc-track-mode)
  (add-hook 'erc-mode-hook 'erc-fill-mode)
  (add-hook 'erc-mode-hook 'erc-ring-mode)
  (add-hook 'erc-mode-hook 'erc-netsplit-mode)
  (add-hook 'erc-mode-hook 'erc-timestamp-mode)
  (add-hook 'erc-mode-hook 'erc-spelling-mode)
  (add-hook 'erc-mode-hook 'erc-notify-mode)
  (add-hook 'erc-mode-hook 'erc-pcomplete-mode)
  (add-hook 'erc-mode-hook 'erc-log-mode)
  (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
  (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)

  (setq erc-modules (append erc-modules '(services notify spelling log)))
  (erc-update-modules)

  (setopt erc-prompt-for-password nil)
  (setopt erc-kill-buffer-on-part t)
  (setopt erc-kill-queries-on-quit nil)
  (setopt erc-kill-server-buffer-on-quit t)
  (setopt erc-auto-query 'window-noselect)
  (setopt erc-keywords '("zmyrgel" "tmy"))

  (setopt erc-track-enable-keybindings t)
  (setopt erc-track-remove-disconnected-buffers t)
  (setopt erc-track-exclude-server-buffer t)
  (setopt erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477"))
  (setopt erc-timestamp-format "[%R-%m/%d]")
  (setopt erc-hide-timestamps nil)

  (pcomplete-erc-setup)

  (setopt erc-pcomplete-order-nickname-completions t)
  (setopt erc-log-channels-directory (locate-user-emacs-file "erc-logs"))
  (setopt erc-log-insert-log-on-open nil)
  (setopt erc-log-file-coding-system 'utf-8-unix)
  (setopt erc-save-buffer-on-part t)
  (setopt erc-max-buffer-size 20000)
  (setopt erc-truncate-buffer-on-save t)
  (defvar erc-insert-post-hook nil))

;;; ftp
(setq ange-ftp-netrc-filename "~/.authinfo.gpg")

;;; browse-url
(setopt browse-url-new-window-flag nil)
(setopt browse-url-firefox-new-window-is-tab t)
(setopt browse-url-browser-function 'eww-browse-url)
(add-to-list 'browse-url-transform-alist
             '("www.google.com" . "www.duckduckgo.com"))
;;;  eww

;; { M-s M-w } eww-search-words
(with-eval-after-load 'eww
  (setopt eww-restore-desktop nil)
  (setopt eww-desktop-remove-duplicates t)
  (setopt eww-header-line-format "%u")
  (setopt eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setopt eww-download-directory (expand-file-name "Downloads" "~"))
  (setopt eww-suggest-uris
          '(eww-links-at-point thing-at-point-url-at-point))
  (setopt eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks"))
  (setopt eww-history-limit 150)
  (setopt eww-use-external-browser-for-content-type
          "\\`\\(video/\\|audio/\\|application/ogg\\|pdf\\)")
  (setopt eww-browse-url-new-window-is-tab nil)
  (setopt eww-form-checkbox-selected-symbol "[X]")
  (setopt eww-form-checkbox-symbol "[ ]")

  (let ((m eww-mode-map))
    (keymap-set m "n" 'next-line)
    (keymap-set m "p" 'previous-line)
    (keymap-set m "f" 'forward-char)
    (keymap-set m "b" 'backward-char)
    (keymap-set m "B" 'eww-back-url)
    (keymap-set m "N" 'eww-next-url)
    (keymap-set m "P" 'eww-previous-url))

  (setopt eww-auto-rename-buffer 'url)
  ;;(setopt shr-use-xwidgets-for-media t) ;; experimental
  )

;;; webjump
(with-eval-after-load 'webjump
  (setopt webjump-sites
          '(("DuckDuckGo" . [simple-query "www.duckduckgo.com" "www.duckduckgo.com/?q=" ""])
            ("Google" . [simple-query "www.google.com" "www.google.com/search?q=" ""])
            ("YouTube" . [simple-query "www.youtube.com/feed/subscriptions" "www.youtube.com/rnesults?search_query=" ""])
            ("ChatGPT" . [simple-query "https://chatgpt.com" "https://chatgpt.com/?q=" ""])
            ("Wikipedia" . [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
            ("Emacs Wiki" . [simple-query "www.emacswiki.org" "www.emacswiki.org/cgi-bin/wiki/" ""]))))
(keymap-global-set "C-x /" 'webjump)

;;; elfeed
(ensure-packages-present 'elfeed)
(with-eval-after-load 'elfeed
  (setopt elfeed-use-curl t)
  (setopt elfeed-curl-max-connections 10)
  (setopt elfeed-db-directory (locate-user-emacs-file "elfeed/"))
  (setopt elfeed-enclosure-default-dir "~/Downloads/")
  (setopt elfeed-search-filter "@1-months-ago +unread")
  (setopt elfeed-sort-order 'descending)
  (setopt elfeed-search-clipboard-type 'CLIPBOARD)
  (setopt elfeed-search-title-max-width 100)
  (setopt elfeed-search-title-min-width 30)
  (setopt elfeed-search-trailing-width 25)
  (setopt elfeed-show-truncate-long-urls t)
  (setopt elfeed-show-unique-buffers t)

  (setopt elfeed-feeds
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


(provide 'init-web)

;;; init-web.el ends here
