;;; init-calendar.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Time keeping related configuration

;;; Code:

(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook 'diary-sort-entries)
(add-hook 'diary-list-entries-hook 'diary-mark-included-diary-files)

(setopt diary-display-function 'diary-fancy-display)
(setopt diary-number-of-entries 7)

(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(setopt calendar-date-style 'european)

(setopt calendar-week-start-day 1)
(setopt calendar-day-name-array
        ["sunnuntai" "maanantai" "tiistai" "keskiviikko"
         "torstai" "perjantai" "lauantai"])
(setopt calendar-month-name-array
        ["tammikuu" "helmikuu" "maaliskuu" "huhtikuu" "toukokuu"
         "kesäkuu" "heinäkuu" "elokuu" "syyskuu"
         "lokakuu" "marraskuu" "joulukuu"])

(setopt calendar-mark-holidays-flag t)
(setopt calendar-view-diary-initially-flag t)
(setopt calendar-mark-diary-entries-flag t)
(setopt diary-show-holidays-flag t)
(setopt diary-file "~/Documents/diary")

(setopt calendar-time-display-form
        '(24-hours ":" minutes
                   (if time-zone " (") time-zone (if time-zone ")")))
(setopt calendar-latitude 60.333847)
(setopt calendar-longitude 25.027310)

;; time utilities
(setopt time-stamp-active t)
(setopt time-stamp-line-limit 10)
(setopt time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S (%l)")

(display-time-mode -1)
(setopt display-time-24hr-format t)
(setopt display-time-day-and-date nil)
(setopt display-time-format nil)
(setopt display-time-use-mail-icon t)

(provide 'init-calendar)

;; init-calendar.el ends here
