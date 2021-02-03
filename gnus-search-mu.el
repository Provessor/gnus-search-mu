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
    (sender (setf (car expr) 'from))
    (recipient (setf (car expr) 'to))
    (mark (setf (car expr) 'tag)))
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
  ;;    ((eql (car expr) 'address)
  ;;     (gnus-search-transform engine `((or (from . ,(cdr expr))
  ;; 					  (to . ,(cdr expr))))))
  ;;    ((eql (car expr 'body))
  ;;     (cdr expr))
  ;;    ((memq (car expr) '(from to subject attachment mimetype tag
  ;; 			      id thread folder path lastmod query
  ;; 			      property))
  ;;     (when (eql (car expr) 'id)
  ;; 	(setf (cdr expr) (replace-regexp-in-string "\\`<\\|>\\'"
  ;; 						   ""
  ;; 						   (cdr expr))))
  ;;     (format "%s:%s" (car expr)
  ;; 	      (if (string-match "\\`\\*" (cdr expr))
  ;; 		  (replace-match "" nil nil (cdr expr))
  ;; 		(cdr expr))))
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
	;;(thread (alist-get 'thread query))
	)
    (with-slots (switches config-directory) engine
      `("find" 			; command must come first
	,(format "--muhome=%s" config-directory)
	,@switches
	,(if limit (format "--maxnum=%d" limit) "")
	,qstring
	,@(if groups
	      `("and" "("
		,@(mapcar (lambda (x)
			    (concat "maildir:/" x))
			  groups)
		")")
	    "")
	"--format=sexp"))))

(cl-defmethod gnus-search-indexed-parse-output ((engine gnus-search-mu)
						server query &optional groups)
  (let ((prefix (slot-value engine 'remove-prefix))
	(group-regexp (when groups
			(mapconcat
			 (lambda (x)
			   (regexp-quote (gnus-group-real-name x)))
			 groups "\\|")))
	artlist vectors article group)
    (goto-char (point-min))
    (while (not (eobp))
      (pcase-let ((`(,f-name ,score) (gnus-search-indexed-extract engine)))
	(when (and (file-readable-p f-name)
		   (null (file-directory-p f-name))
		   (or (null groups)
		       (and (gnus-search-single-p query)
			    (alist-get 'thread query))
		       (string-match-p group-regexp f-name)))
	  (push (list f-name score) artlist))))
    ;; Are we running an additional grep query?
    (when-let ((grep-reg (alist-get 'grep query)))
      (setq artlist (gnus-search-grep-search engine artlist grep-reg)))
    ;; Prep prefix.
    (when (and prefix (null (string-empty-p prefix)))
      (setq prefix (file-name-as-directory (expand-file-name prefix))))
    ;; Turn (file-name score) into [group article score].
    (pcase-dolist (`(,f-name ,score) artlist)
      (setq article (file-name-nondirectory f-name)
	    group (file-name-directory f-name))
      ;; Remove prefix.
      (when prefix
	(setq group (string-remove-prefix prefix group)))
      ;; Break the directory name down until it's something that
      ;; (probably) can be used as a group name.
      (setq group
	    (replace-regexp-in-string
	     "[/\\]" "."
	     (replace-regexp-in-string
	      "/?\\(cur\\|new\\|tmp\\)?/\\'" ""
	      (replace-regexp-in-string
	       "^[./\\]" ""
	       group nil t)
	      nil t)
	     nil t))

      (push (vector (gnus-group-full-name group server)
		    (if (string-match-p "\\`[[:digit:]]+\\'" article)
			(string-to-number article)
		      (nnmaildir-base-name-to-article-number
		       (substring article 0 (string-match ":" article))
		       group (string-remove-prefix "nnmaildir:" server)))
		    (if (numberp score)
			score
		      (string-to-number score)))
	    vectors))
    vectors))

(provide 'gnus-search-mu)
