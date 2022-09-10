;;; ------------------------------
;;; Calendar and diary settings
;;; ------------------------------

(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook 'diary-sort-entries)
(add-hook 'diary-list-entries-hook 'diary-mark-included-diary-files)

(setq diary-display-function 'diary-fancy-display)
(setq diary-number-of-entries 7)

(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(setq calendar-date-style 'european)

(setq calendar-week-start-day 1)
(setq calendar-day-name-array
      ["sunnuntai" "maanantai" "tiistai" "keskiviikko"
       "torstai" "perjantai" "lauantai"])
(setq calendar-month-name-array
      ["tammikuu" "helmikuu" "maaliskuu" "huhtikuu" "toukokuu"
       "kesäkuu" "heinäkuu" "elokuu" "syyskuu"
       "lokakuu" "marraskuu" "joulukuu"])

(setq calendar-mark-holidays-flag t)
(setq calendar-view-diary-initially-flag t)
(setq calendar-mark-diary-entries-flag t)
(setq diary-show-holidays-flag t)
(setq diary-file "/ssh:tmy@mars.bittivirhe.fi:diary")

(setq calendar-latitude 60.29414
      calendar-longitude 25.04099)

;; time utilities
(setq time-stamp-active t)
(setq time-stamp-line-limit 10)
(setq time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S (%u)")

(display-time-mode -1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date nil)
(setq display-time-format nil)
(setq display-time-use-mail-icon t)

(zmg/package-install 'suomalainen-kalenteri)

(provide 'zmg-emacs-calendar)
