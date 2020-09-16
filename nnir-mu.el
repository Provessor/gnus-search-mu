;;; nnmu.el --- mu backend for nnir -*- lexical-binding: t -*-

;; Copyright (C) 2020 Jai Flack

;; Author: Jai Flack <jflack@disroot.org>
;; Version: 2020-09-15
;; URL: https://git.disroot.org/jflack/nnir-mu
;; Package-Requires: ((emacs "24.1"))
;; Keywords: Gnus nnir mu

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

;; This package provides an `mu' backend for Gnus' nnir as an
;; alternative to notmuch or any other backends. It relies on the `mu'
;; executable and thus can only search local mail and requires `mu' to
;; be configured for your mail setup. Tested with an nnmaildir backend
;; synced with mbsync.

;;; Configuration:

;; Generally the defaults will work find but first mu must be
;; installed and configured, see mu(1) and mu-init(1).

;; Available options:
;;  - `nnir-mu-program'
;;  - `nnir-mu-remove-prefix'

;;; Code:

(require 'nnir)


(defcustom nnir-mu-program "mu"
  "Name of mu search executable."
  :type '(string)
  :group 'nnir)

(defcustom nnir-mu-additional-switches '()
  "A list of strings, to be given as additional arguments to mu."
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-mu-remove-prefix nil
  "The prefix to remove from each file name returned by notmuch
in order to get a group name. Generally this should be set to
your path to your mail directory. This is a regular expression.

If it is `nil' then the maildir returned from mu will be used
instead. This can be an expensive process but works without any
configuration.

This is very similar to `nnir-notmuch-remove-prefix' and
`nnir-namazu-remove-prefix'."
  :type '(regexp)
  :group 'nnir)

;; TODO: part of notmuch engine
;; (defcustom nnir-mu-filter-group-names-function nil)


(defun nnir-run-mu (query server &optional groups)
  "Run QUERY against mu."
  (message "nnir-run-mu: ")
  (save-excursion
    (let* (artlist
	   (qstring (cdr (assq 'query query)))
	   (prefix (nnir-read-server-parm 'nnir-mu-remove-prefix server))
	   (article-pattern (if (string-prefix-p "nnmaildir:"
						 (gnus-group-server server))
				":[0-9]+"
			      "^[0-9]+$")))
      (when (string-equal "" qstring)
	(error "mu: You need a search term"))

      (set-buffer (gnus-get-buffer-create nnir-tmp-buffer))
      (erase-buffer)

      (let ((cp-list `(,nnir-mu-program
		       nil
		       t
		       nil
		       "find"
		       "--format=sexp"
		       ,@(nnir-read-server-parm 'nnir-mu-additional-switches
						server)
		       ,qstring)))
	(apply #'call-process cp-list))

      (goto-char (point-min))
      (let (point-start objcons filenam artno dirnam)
	(while (not (looking-at "\n$"))
	  (setq objcons (car
			 (read-from-string
			  (decode-coding-string
			   (buffer-substring-no-properties
			    (1- (search-forward-regexp "^(" nil t))
			    (search-forward-regexp "^)" nil t))
			   'utf-8 t))))
	  (setq filenam (plist-get objcons :path)
		artno (file-name-nondirectory filenam)
		dirnam (file-name-directory filenam))

	  (when (and (string-match article-pattern artno)
		     (not (null dirnam)))
	    (unless prefix
		(setq prefix (string-trim-right filenam
						(concat (regexp-quote
							 (plist-get objcons
								    :maildir))
							".*"))))
	    (print (list dirnam artno "" prefix server artlist))
	    (nnir-add-result dirnam artno "" prefix server artlist))))

      (message "Getting massaged by mu...done")

      artlist)))


(add-to-list 'nnir-engines '(mu nnir-run-mu
				()))

(provide 'nnir-mu)
