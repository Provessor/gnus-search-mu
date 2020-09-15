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

;; This package provides an `mu' backend for Gnus' nnir as an alternative to notmuch or any other
;; backends. It relies on the `mu' executable and thus can only search local mail and requires `mu'
;; to be configured for your mail setup. Tested with an nnmaildir backend synced with mbsync.

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

(defcustom nnir-mu-remove-prefix
  (regexp-quote (or (getenv "MAILDIR") (expand-file-name "~/Mail")))
  "The prefix to remove from each file name returned by notmuch
in order to get a group name. Generally this should be set to
your path to your mail directory. This is a regular expression.

This is very similar to `nnir-notmuch-remove-prefix' and
`nnir-namazu-remove-prefix'."
  :type '(regexp)
  :group 'nnir)

(defcustom nnir-mu-filter-group-names-function nil)


(defun nnir-run-mu (query server &optional groups)
  "Run QUERY against mu.")


(add-to-list 'nnir-engines '(mu nnir-run-mu
				()))

(provide 'nnir-mu)
