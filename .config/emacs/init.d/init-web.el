;;; init-web.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Init web-related things
;;; - Need for ERC config at all?

;;; Code:

;; rcirc
(with-eval-after-load 'rcirc
  (setq rcirc-server-alist
	'(("irc.libera.chat"
           :channels ("#openbsd" "#lisp")
           :port 6697
           :encryption tls)))
  (setq rcirc-default-nick "zmyrgel")
  (setq rcirc-default-user-name "zmyrgel")
  (setq rcirc-default-full-name "Curious Minds Want To Know")

  (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
  (rcirc-omit-mode 1)

  (rcirc-track-minor-mode 1)

  (when-let* ((nickserv-pass (password-lookup :host "irc.libera.chat")))
    (setq rcirc-authinfo
          `(("libera" nickserv "zmyrgel" ,nickserv-pass))))

  (setq rcirc-time-format "%Y-%m-%d %H:%M ")
  (setq rcirc-log-time-format "%Y-%m-%d %H:%M "))

;;  erc
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

(ensure-packages-present 'elfeed)
(with-eval-after-load 'elfeed
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory (locate-user-emacs-file "elfeed/"))
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@1-months-ago +unread")
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
;;  eww
;; :commands (eww
;;            eww-browse-url
;;            eww-search-words
;;            eww-open-in-new-buffer
;;            eww-open-file)

(with-eval-after-load 'eww
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

  (let ((m eww-mode-map))
    (define-key m "n" 'next-line)
    (define-key m "p" 'previous-line)
    (define-key m "f" 'forward-char)
    (define-key m "b" 'backward-char)
    (define-key m "B" 'eww-back-url)
    (define-key m "N" 'eww-next-url)
    (define-key m "P" 'eww-previous-url))

  (setq browse-url-new-window-flag nil)
  (setq browse-url-firefox-new-window-is-tab t)
  (setq browse-url-browser-function 'eww-browse-url)
  (setq eww-auto-rename-buffer 'url)
  ;;(setq shr-use-xwidgets-for-media t) ;; experimental
  )

(ensure-packages-present 'restclient)
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
;;TODO: change to only apply json formatting when the content-type is
;;application/json
(with-eval-after-load 'restclient
  (define-key restclient-mode-map (kbd "C-c C-f") 'json-mode-beautify))

(with-eval-after-load 'webjump
  (setq webjump-sites
        '(("DuckDuckGo" . [simple-query "www.duckduckgo.com" "www.duckduckgo.com/?q=" ""])
          ("Google" . [simple-query "www.google.com" "www.google.com/search?q=" ""])
          ("YouTube" . [simple-query "www.youtube.com/feed/subscriptions" "www.youtube.com/rnesults?search_query=" ""])
          ("ChatGPT" . [simple-query "https://chatgpt.com" "https://chatgpt.com/?q=" ""])
          ("Wikipedia" . [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
          ("Emacs Wiki" . [simple-query "www.emacswiki.org" "www.emacswiki.org/cgi-bin/wiki/" ""]))))

(global-set-key (kbd "C-x /") 'webjump)

(provide 'init-web)

;; init-web.el ends here
