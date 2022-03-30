;;; gnus-search-mu.el --- mu backend for gnus-search -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Jai Flack
;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Jai Flack <jflack@disroot.org>
;; Version: 1.0
;; URL: https://git.disroot.org/jflack/gnus-search-mu
;; Package-Requires: ((emacs "28.1"))
;; Keywords: Gnus gnus-search mu

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an `mu' engine for Gnus' search facility as an
;; alternative to notmuch or any other backends. It relies on the `mu'
;; executable and thus can only search local mail and requires `mu' to
;; be configured for your mail setup. Tested with an nnmaildir backend
;; synced with mbsync.

;;; Configuration:

;; Generally the defaults will work fine but first mu must be installed
;; and configured, see mu(1) and mu-init(1). All these options can be
;; configured per-server as documented in the manual (removing the
;; `gnus-search-mu' prefix of course).

;; Available options:
;;  - `gnus-search-mu-program'
;;  - `gnus-search-mu-switches'
;;  - `gnus-search-mu-remove-prefix'
;;  - `gnus-search-mu-config-directory'
;;  - `gnus-search-mu-raw-queries-p'


;;; Code:

(require 'gnus-search)
(require 'gnus-art)


(defcustom gnus-search-mu-program "mu"
  "Name of mu search executable.
This can also be set per-server."
  :type '(string)
  :group 'gnus-search)

(defcustom gnus-search-mu-switches '()
  "A list of strings, to be given as additional arguments to mu.
Note that this should be a list. I.e., do NOT use the following:
    (setq gnus-search-mu-switches \"-u -r\")
Instead, use this:
    (setq gnus-search-mu-switches \\='(\"-u\" \"-r\"))
This can also be set per-server."
  :type '(repeat (string))
  :group 'gnus-search)

(defcustom gnus-search-mu-remove-prefix (expand-file-name "Mail/" "~")
  "A prefix to remove from the mu results to get a group name.
Usually this will be set to the path to your mail directory. This
can also be set per-server."
  :type '(directory)
  :group 'gnus-search)

(defcustom gnus-search-mu-config-directory
  (expand-file-name "~/.cache/mu")
  "Configuration directory for mu.
This can also be set per-server."
  :type 'file
  :group 'gnus-search)

(defcustom gnus-search-mu-raw-queries-p nil
  "If t, all mu engines will only accept raw search query strings.
This can also be set per-server."
  :type 'boolean
  :group 'gnus-search)

(defclass gnus-search-mu (gnus-search-indexed)
  ((program
    :initform (symbol-value 'gnus-search-mu-program))
   (remove-prefix
    :initform (symbol-value 'gnus-search-mu-remove-prefix))
   (switches
    :initform (symbol-value 'gnus-search-mu-switches))
   (config-directory
    :initform (symbol-value 'gnus-search-mu-config-directory))
   (raw-queries-p
    :initform (symbol-value 'gnus-search-mu-raw-queries-p))))


(cl-defmethod gnus-search-transform-expression ((engine gnus-search-mu)
						(expr list))
  (cl-case (car expr)
    (recipient (setf (car expr) 'recip))
    (address (setf (car expr) 'contact))
    (id (setf (car expr) 'msgid))
    (attachment (setf (car expr) 'file)))
  (cl-flet ()
    (cond
     ((consp (car expr))
      (format "(%s)" (gnus-search-transform engine expr)))
     ;; Explicitly leave out 'date as gnus-search will encode it
     ;; first; it is handled later
     ((memq (car expr) '(cc c bcc h from f to t subject s body b
			    maildir m msgid i prio p flag g d
			    size z embed e file j mime y tag x
			    list v))
      (format "%s:%s" (car expr)
	      (if (string-match "\\`\\*" (cdr expr))
		  (replace-match "" nil nil (cdr expr))
		(cdr expr))))
     ((eq (car expr) 'mark)
      (format "flag:%s" (gnus-search-mu-handle-flag (cdr expr))))
     ((eq (car expr) 'date)
      (format "date:%s" (gnus-search-mu-handle-date (cdr expr))))
     ((eq (car expr) 'before)
      (format "date:..%s" (gnus-search-mu-handle-date (cdr expr))))
     ((eq (car expr) 'since)
      (format "date:%s.." (gnus-search-mu-handle-date (cdr expr))))
     (t (ignore-errors (cl-call-next-method))))))

(defun gnus-search-mu-handle-date (date)
  (if (stringp date)
      date
    (pcase date
      (`(nil ,m nil)
       (nth (1- m) gnus-english-month-names))
      (`(nil nil ,y)
       (number-to-string y))
      ;; mu prefers ISO date YYYY-MM-DD HH:MM:SS
      (`(,d ,m nil)
       (let* ((ct (decode-time))
	      (cm (decoded-time-month ct))
	      (cy (decoded-time-year ct))
	      (y (if (> cm m)
		     cy
		   (1- cy))))
	 (format "%d-%02d-%02d" y m d)))
      (`(nil ,m ,y)
       (format "%d-%02d" y m))
      (`(,d ,m ,y)
       (format "%d-%02d-%02d" y m d)))))

(defun gnus-search-mu-handle-flag (flag)
  ;; Only change what doesn't match
  (cond ((string= flag "flag")
	 "flagged")
	((string= flag "read")
	 "seen")
	(t
	 flag)))


(cl-defmethod gnus-search-indexed-extract ((_engine gnus-search-mu))
  (prog1
      (let ((bol (line-beginning-position))
	    (eol (line-end-position)))
	(list (buffer-substring-no-properties bol eol)
	      100))
    (move-beginning-of-line 2)))

(cl-defmethod gnus-search-indexed-search-command ((engine gnus-search-mu)
						  (qstring string)
						  query &optional groups)
  (let ((limit (alist-get 'limit query))
	(thread (alist-get 'thread query)))
    (with-slots (switches config-directory) engine
      `("find" 			; command must come first
	"--nocolor"		; mu will always give coloured output otherwise
	,(format "--muhome=%s" config-directory)
	,@switches
	,(if thread "-r" "")
	,(if limit (format "--maxnum=%d" limit) "")
	,qstring
	,@(if groups
	      `("and" "("
		,@(nbutlast (mapcan (lambda (x)
				      (list (concat "maildir:/" x) "or"))
				    groups))
		")")
	    "")
	"--format=plain"
	"--fields=l"))))

(provide 'gnus-search-mu)
