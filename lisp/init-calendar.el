;;; init-calendar.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package calendar
  :defer 1
  :custom (calendar-mark-holidays-flag t))

(use-package holidays
  :disabled t
  :defer 1
  :ensure nil
  :custom
  (holiday-bahai-holidays nil)
  (holiday-christian-holidays
   '((holiday-fixed 1 6 "Epiphany")
     (holiday-fixed 2 2 "Candlemas")
     (holiday-easter-etc -47 "Mardi Gras")
     (holiday-easter-etc 0 "Easter Day")
     (holiday-easter-etc 1 "Easter Monday")
     (holiday-easter-etc 39 "Ascension")
     (holiday-easter-etc 49 "Pentecost")
     (holiday-fixed 8 15 "Assumption")
     (holiday-fixed 11 1 "All Saints' Day")
     (holiday-fixed 11 2 "Day of the Dead")
     (holiday-fixed 11 22 "Saint Cecilia's Day")
     (holiday-fixed 12 1 "Saint Eloi's Day")
     (holiday-fixed 12 4 "Saint Barbara")
     (holiday-fixed 12 6 "Saint Nicholas Day")
     (holiday-fixed 12 25 "Christmas Day")))
  (holiday-general-holidays
   '((holiday-fixed 1 1 "New Year's Day")
     (holiday-fixed 2 14 "Valentine's Day")
     (holiday-fixed 3 8 "International Women's Day")
     (holiday-fixed 10 31 "Halloween")
     (holiday-fixed 11 11 "Armistice of 1918")))
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-local-holidays
   '((holiday-fixed 5 1 "Labor Day")
     (holiday-float 3 0 0 "Grandmothers' Day")
     (holiday-float 4 4 3 "Secretary's Day")
     (holiday-float 5 0 2 "Mother's Day")
     (holiday-float 6 0 3 "Father's Day")))
  (holiday-oriental-holidays nil))

(provide 'init-calendar)
;;; init-calendar.el ends here
