;;; gnus-search-mu.el --- mu backend for gnus-search -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jai Flack

;; Author: Jai Flack <jflack@disroot.org>
;; Version: 2020-09-15
;; URL: https://git.disroot.org/jflack/gnus-search-mu
;; Package-Requires: ((emacs "28.1"))
;; Keywords: Gnus gnus-search mu

;; This file is NOT part of GNU Emacs.

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

;;; Commentary

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

Changing the format will have no effect because it is forced to
\"--format=sexp\".

This can also be set per-server."
  :type '(repeat (string))
  :group 'gnus-search)

(defcustom gnus-search-mu-remove-prefix (expand-file-name "Mail/" (getenv "HOME"))
  "The prefix to remove from each file name returned by mu in
order to get a group name. Generally this should be set to your
path to your mail directory. This is a regular expression.

This is very similar to `gnus-search-notmuch-remove-prefix' and
`gnus-search-namazu-remove-prefix'."
  :type '(regexp)
  :group 'gnus-search)

(defcustom gnus-search-mu-config-directory
  (expand-file-name "~/.mu")
  "Configuration directory for mu.

This can also be set per-server."
  :type 'file
  :group 'gnus-search)

(defcustom gnus-search-mu-raw-queries-p nil
  "If t, all mu engines will only accept raw search query
strings.

This is very similar to `gnus-search-notmuch-raw-queries-p'
and `gnus-search-namazu-raw-queries-p'.

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


;; (cl-defmethod gnus-search-transform ((_engine gnus-search-mu)
;; 				     (_query null))
;;   "*")

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-mu)
						(expr (head near)))
  (format "%s near %s"
	  (gnus-search-transform-expression engine (nth 1 expr))
	  (gnus-search-transform-expression engine (nth 2 expr))))

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-mu)
						(expr list))
  (cl-case (car expr)
    (recipient (setf (car expr) 'recip))
    (address (setf (car expr) 'contact))
    (id (setf (car expr) 'msgid))   	; message-id is translated
    ;; mark
    (attachment (setf (car expr) 'file))
    ;; TODO: implement or remove these
    (thread (error "thread: not yet implemented")))
  (cl-flet ((mu-date (date)
		     (if (stringp date)
			 date
		       (pcase date
			 (`(nil ,m nil)
			  (nth (1- m) gnus-english-month-names))
			 (`(nil nil ,y)
			  (number-to-string y))
			 (`(,d ,m nil)
			  (format "%02d-%02d" d m))
			 (`(nil ,m ,y)
			  (format "%02d-%02d" m y))
			 (`(,d ,m ,y)
			  (format "%d/%d/%d" m d y))))))
    (cond
     ((consp (car expr))
      (format "(%s)" (gnus-search-transform engine expr)))
     ((memq (car expr) '(cc c bcc h from f to t subject s body b
			    maildir m msgid i prio p flag g date d
			    size z embed e file j mime y tag x
			    list v))
      ;; TODO: check if msgid needs translation
      ;; (when (eql (car expr) 'id)
      ;; 	(setf (cdr expr) (replace-regexp-in-string "\\`<\\|>\\'"
      ;; 						   ""
      ;; 						   (cdr expr))))
      (format "%s:%s" (car expr)
	      (if (string-match "\\`\\*" (cdr expr))
		  (replace-match "" nil nil (cdr expr))
		(cdr expr))))
     ((eq (car expr) 'date)
      (format "date:%s" (mu-date (cdr expr))))
     ((eq (car expr) 'before)
      (format "date:..%s" (mu-date (cdr expr))))
     ((eq (car expr) 'since)
      (format "date:%s.." (mu-date (cdr expr))))
     (t (ignore-errors (cl-call-next-method))))))

(cl-defmethod gnus-search-indexed-extract ((_engine gnus-search-mu))
  (let ((objcons (car (read-from-string
		       (decode-coding-string
			(buffer-substring-no-properties
			 (1- (search-forward-regexp "^("))
			 (search-forward-regexp "^)"))
			'utf-8 t)))))
    (when (looking-at "\n$")
      (goto-char (point-max)))
    (list (plist-get objcons :path) 100)))

(cl-defmethod gnus-search-indexed-search-command ((engine gnus-search-mu)
						  (qstring string)
						  query &optional groups)
  (let ((limit (alist-get 'limit query))
	;; (thread (alist-get 'thread query))
	)
    (with-slots (switches config-directory) engine
      `("find" 			; command must come first
	,(format "--muhome=%s" config-directory)
	,@switches
	,(if limit (format "--maxnum=%d" limit) "")
	,qstring
	,@(if groups
	      `("and" "("
		,@(nbutlast (mapcan (lambda (x)
				      (list (concat "maildir:/" x) "or"))
				    groups))
		")")
	    "")
	"--format=sexp"))))

(provide 'gnus-search-mu)
