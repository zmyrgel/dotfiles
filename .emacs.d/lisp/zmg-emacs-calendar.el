;;; ------------------------------
;;; Calendar and diary settings
;;; ------------------------------

(use-package diary-lib
  :hook ((diary-list-entries-hook . diary-include-other-diary-files)
         (diary-list-entries-hook . diary-sort-entries)
         (diary-list-entries-hook . diary-mark-included-diary-files))
  :config
  (setq diary-display-function 'diary-fancy-display)
  (setq diary-number-of-entries 7))

(use-package calendar
  :hook (calendar-today-visible-hook . calendar-mark-today)
  :init (setq calendar-date-style 'european)
  :config
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
  (setq diary-file "/ssh:tmy@mars.bittivirhe.fi:diary"))

(use-package solar
  :config
  (setq calendar-latitude 60.29414
        calendar-longitude 25.04099))

;; time utilities
(use-package time-stamp
  :config
  (setq time-stamp-active t)
  (setq time-stamp-line-limit 10)
  (setq time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S (%u)"))

(use-package time
  :config
  (display-time-mode -1)
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date nil)
  (setq display-time-format nil)
  (setq display-time-use-mail-icon t))

(use-package suomalainen-kalenteri
  :ensure t)

(provide 'zmg-emacs-calendar)
