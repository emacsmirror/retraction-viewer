;;; retraction-viewer.el --- View retraction information for current citation  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (plz "0.7"))
;; Keywords: bib, tex, data
;; URL: https://git.sr.ht/~swflint/retraction-viewer
;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2024 Samuel W. Flint <swflint@flintfam.org>

;; This program is free software; you can redistribute it and/or modify
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
;;
;; TODO

;;; Code:
(require 'plz)


;;; Customization

(defgroup retraction-viewer nil
  "Customization of retraction-viewer."
  :group 'bibtex
  :prefix "retraction-viewer-"
  :link '(url-link :tag "Sourcehut" "https://git.sr.ht/~swflint/retraction-viewer")
  :link '(emacs-library-link :tag "Library Source" "retraction-viewer.el"))

(defcustom retraction-viewer-crossref-email nil
  "Email to use for authentication to crossref API."
  :group 'retraction-viewer
  :type '(choice (string :tag "Email:")
                 (const :tag "Authentication Disabled (warning will be issued)" nil)))

(defcustom retraction-viewer-doi-functions (list #'retraction-viewer-get-ebib-doi
                                                 #'retraction-viewer-get-bibtex-doi)
  "How should a DOI be gotten?

This is a list of functions, run until one returns non-nil."
  :group 'retraction-viewer
  :type 'hook)


;;; Utility Functions

(defun retraction-viewer--format-url (doi)
  "Format data URL for DOI.

Note, `retraction-viewer-crossref-email' must be set."
  (if (null retraction-viewer-crossref-email)
      (display-warning 'retraction-viewer "Please set `retraction-viewer-crossref-email' to a string value" :error)
    (format-spec "https://api.labs.crossref.org/works/%d?mailto=%m"
                 `((?d . ,doi)
                   (?m . ,retraction-viewer-crossref-email)))))

(define-hash-table-test 'retraction-watch-string=
                        'string= 'sxhash-equal)

(defvar retraction-viewer--cached-retraction-status
  (make-hash-table :test 'retraction-watch-string=)
  "Cached results for retraction viewer.")

(defun retraction-viewer--get-retraction-status (doi)
  "Get the retraction status of DOI."
  (or (gethash doi retraction-viewer--cached-retraction-status)
      (when-let* ((url (retraction-viewer--format-url doi))
                  (data (plz 'get url :as #'json-read))
                  (message (alist-get 'message data))
                  (updates (cl-map 'list #'identity (alist-get 'cr-labs-updates message)))
                  (retraction-messages (cl-remove-if-not (lambda (entry)
                                                           (when-let* ((about (alist-get 'about entry))
                                                                       (source-url (alist-get 'source_url about)))
                                                             (string= "https://retractionwatch.com" source-url)))
                                                         updates)))
        (puthash doi retraction-messages
                 retraction-viewer--cached-retraction-status))))


;;; Get current DOI

(defun retraction-viewer-get-current-doi ()
  "Get the current DOI, using `retraction-viewer-doi-functions'."
  (run-hook-with-args-until-success 'retraction-viewer-doi-functions))

(declare-function ebib-get-field-value "ebib-utils.el")
(declare-function ebib--get-key-at-point "ebib.el")
(defvar ebib--cur-db)
(defun retraction-viewer-get-ebib-doi ()
  "Get DOI from current Ebib entry."
  (when (and (featurep 'ebib)
             (derived-mode-p 'ebib-entry-mode 'ebib-index-mode))
    (ebib-get-field-value "doi" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced)))

(defun retraction-viewer-get-bibtex-doi ()
  "Get DOI from current BibTeX entry."
  (when (derived-mode-p 'bibtex-mode)
    (bibtex-text-in-field "doi")))

(provide 'retraction-viewer)

;;; retraction-viewer.el ends here
