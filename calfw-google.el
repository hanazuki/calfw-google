;; -*- coding: utf-8; lexical-binding -*-
;;; calfw-google.el --- Google Calendar integration on calfw

;; Copyright (C) 2014 Kasumi Hanazuki

;; Author: Kasumi Hanazuki <kasumi at rollingapple.net>
;; Version: 0.0.1

(require 's)
(require 'calfw)
(require 'dash)
(require 'json)
(require 'oauth2)

(defconst cfw-google:oauth2-auth-url "https://accounts.google.com/o/oauth2/auth")
(defconst cfw-google:oauth2-token-url "https://accounts.google.com/o/oauth2/token")
(defconst cfw-google:calendar-scope-url "https://www.googleapis.com/auth/calendar")
(defconst cfw-google:calendar-readonly-scope-url "https://www.googleapis.com/auth/calendar.readonly")
(defconst cfw-google:calendar-endpoint-url "https://www.googleapis.com/calendar/v3")

(defcustom cfw-google:oauth2-client-id "917216134153-2cd3bsd89bumrahlou11n6jfeinu7c83.apps.googleusercontent.com"
  "client_id"
  :group 'cfw
  :type 'string)

(defcustom cfw-google:oauth2-client-secret "Qn3ivwmxr2VcglQvwqx-w4Nt"
  "client_secret"
  :group 'cfwnc
  :type 'string)

(defvar-local cfw-google:-oauth2-token nil)

(defun cfw-google:auth ()
  (or cfw-google:-oauth2-token
      (progn
        (setq cfw-google:-oauth2-token
              (oauth2-auth-and-store
                    cfw-google:oauth2-auth-url cfw-google:oauth2-token-url cfw-google:calendar-scope-url
                    cfw-google:oauth2-client-id cfw-google:oauth2-client-secret))
        cfw-google:-oauth2-token)))

(defun cfw-google:oauth2-url-retrieve-synchronously (url &rest args)
  (with-current-buffer (get-buffer-create "*cfw-google*")
    (let ((token (cfw-google:auth)))
      (insert url)
      (newline)
      (apply #'oauth2-url-retrieve-synchronously `(,token ,url . ,args)))))


(defun cfw-google:calendar-api-url (path &optional params)
  (concat
   cfw-google:calendar-endpoint-url "/"
   (if (listp path) (mapconcat #'url-hexify-string path "/") path) "?"
   (mapconcat
    (lambda (key-value)
        (concat
         (url-hexify-string (car key-value))
         "="
         (url-hexify-string (cdr key-value))))
    (-filter #'cdr params) "&")))

(defun cfw-google:http-json-read-buffer (buffer)
  (display-buffer buffer) ; debug
  (json-read-from-string
   (with-current-buffer buffer
     (decode-coding-string
      (buffer-substring (1+ url-http-end-of-headers) (point-max))
      'utf-8)))) ; TODO: precise charset handling

(defun cfw-google:calendarlist-id (json)
  (cdr (assoc 'id json)))

(defun cfw-google:calendarlist-summary (json)
  (cdr (assoc 'summary json)))


(defun cfw-google:-parse-datetime (json)
  (cond
   ((assoc 'date json)
    (cons (cfw-google:-parse-iso8601-date (cdr (assoc 'date json))) nil))
   ((assoc 'dateTime json)
    (cfw-google:-parse-iso8601-datetime (cdr (assoc 'dateTime json))))))

(defun cfw-google:-parse-event (json)
  (let ((start (cfw-google:-parse-datetime (cdr (assoc 'start json))))
        (end (cfw-google:-parse-datetime (cdr (assoc 'end json)))))
    (make-cfw:event
     :title (cdr (assoc 'summary json))
     :description (cdr (assoc 'description json))
     :location (cdr (assoc 'location json))
     :start-date (car start)
     :start-time (cdr start)
     :end-date (car end)
     :end-time (cdr end))))

(defun cfw-google:api-calendarlist-list (&optional page-token)
  (let* ((url (cfw-google:calendar-api-url
               (list "users" "me" "calendarList")
               (list (cons "pageToken" page-token))))
         (response (cfw-google:http-json-read-buffer
                    (cfw-google:oauth2-url-retrieve-synchronously url)))
         (items (cdr (assoc 'items response)))
         (page-token (cdr (assoc 'nextPageToken response)))
         (next-items (and page-token (cfw-google:api-calendarlist-list page-token))))
      (append items next-items)))

(defun cfw-google:api-calendarlist-get (calendar-id)
  (let* ((url (cfw-google:calendar-api-url
               (list "users" "me" "calendarList" calendar-id)))
         (response (cfw-google:http-json-read-buffer
                    (cfw-google:oauth2-url-retrieve-synchronously url))))
      response))


(defun cfw-google:api-events-list (calendar-id time-min time-max &optional page-token)
  (let* ((url (cfw-google:calendar-api-url
               (list "calendars" calendar-id "events")
               (list (cons "timeMin" (and time-min (cfw-google:-time-to-iso8601 time-min)))
                     (cons "timeMax" (and time-max (cfw-google:-time-to-iso8601 time-max)))
                     (cons "pageToken" page-token))))
         (response (cfw-google:http-json-read-buffer
                    (cfw-google:oauth2-url-retrieve-synchronously url)))
         (items (cdr (assoc 'items response)))
         (page-token (cdr (assoc 'nextPageToken response)))
         (next-items (and page-token (cfw-google:api-events-list page-token))))
      (append items next-items)))

(defun cfw-google:-make-cal-date (year month day)
  (cfw:date month day year))

(defun cfw-google:-make-cal-time (hours minutes)
  (cfw:time hours minutes))

(defun cfw-google:-time-to-iso8601 (time)
  (format-time-string "%Y-%m-%dT%T%z" time))

(defun cfw-google:-iso8601-to-time (str)
  (date-to-time str))

(defun cfw-google:-parse-iso8601-datetime (str)
  (let ((m (cdr (s-match "\\`\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)T\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)\\(Z\\|\\([-+]\\)\\([0-9]+\\):\\([0-9]+\\)\\)\\'" str))))
    (when m
      (let* ((tz (if (eq (nth 6 m) "Z") 0
                   (* (if (eq (nth 7 m) "-") -1 +1)
                      (+ (* 60 60 (string-to-number (nth 8 m)))
                         (* 60 (string-to-number (nth 9 m)))))))
             (dt (mapcar #'string-to-number (-select-by-indices '(5 4 3 2 1 0) m)))
             (dt (decode-time (apply #'encode-time `(,@dt ,tz)))))
        (cons
         (apply #'cfw-google:-make-cal-date (-select-by-indices '(5 4 3) dt))
         (apply #'cfw-google:-make-cal-time (-select-by-indices '(2 1) dt)))))))

(defun cfw-google:-parse-iso8601-date (str)
  (let ((m (cdr (s-match "\\`\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)\\'" str))))
    (when m
      (apply #'cfw-google:-make-cal-date (mapcar #'string-to-number m)))))


(defun cfw-google:calendar-update (calendar))

(defun cfw-google:calendar-fetch (calendar begin end)
  (let* ((id (cfw-google:calendarlist-id calendar))
         (begin (cfw:calendar-to-emacs begin))
         (end (cfw:calendar-to-emacs end))
         (events (cfw-google:api-events-list id begin end)))
    (mapcar #'cfw-google:-parse-event events)))


(defun cfw-google:create-source (id)
  (let ((calendar (cfw-google:api-calendarlist-get id)))
    (make-cfw:source
     :name (cfw-google:calendarlist-summary calendar)
     :update (-partial #'cfw-google:calendar-update calendar)
     :data (-partial #'cfw-google:calendar-fetch calendar))))

(provide 'calfw-google)

;;; calfw-google.el ends here
