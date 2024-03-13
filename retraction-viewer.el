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
                                                 #'retraction-viewer-get-bibtex-doi
                                                 #'retraction-viewer-doi-at-point)
  "How should a DOI be gotten?

This is a list of functions, run until one returns non-nil."
  :group 'retraction-viewer
  :type 'hook)

(defcustom retraction-viewer-format-spec
  '((?n . retraction-viewer--format-update-nature)
    (?d . retraction-viewer--format-update-date)
    (?r . retraction-viewer--format-reasons)
    (?u . retraction-viewer--format-target-doi)
    (?D . retraction-viewer--format-doi)
    (?U . retraction-viewer--format-urls)
    (?N . retraction-viewer--format-notes)
    (?a . retraction-viewer--format-asserted-by))
  "Metacharacters for formatting retraction notices.

Keys should be unique characters, and values should be either
references to variables or named functions which take a single
argument, the retraction notice data structure (alist)."
  :group 'retraction-viewer
  :type '(alist :key-type (character :tag "Format Character")
                :value-type (sexp :tag "Getter")))

(defcustom retraction-viewer-notice-format "%n (%d): %r (see also [[%u][%D]])."
  "Default format for retraction notices.

See also `retraction-viewer-format-spec' for available keys."
  :group 'retraction-viewer
  :type 'string)


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


;;; Get DOI Retraction Status

(defun retraction-viewer-doi-status (doi)
  "Get the retraction status of DOI."
  (or (gethash doi retraction-viewer--cached-retraction-status)
      (when-let* ((url (retraction-viewer--format-url doi))
                  (data (plz 'get url :as #'json-read))
                  (message (alist-get 'message data))
                  ;; TODO: Save and add title as well?
                  (updates (cl-map 'list #'identity (alist-get 'cr-labs-updates message)))
                  (retraction-messages (cl-remove-if-not (lambda (entry)
                                                           (when-let* ((about (alist-get 'about entry))
                                                                       (source-url (alist-get 'source_url about)))
                                                             (string= "https://retractionwatch.com" source-url)))
                                                         updates)))
        (puthash doi retraction-messages
                 retraction-viewer--cached-retraction-status))))


;;; Get current DOI

(defconst retraction-viewer-doi-regexp
  (rx (group-n 1 (or (and "10." (>= 4 digit) "/" (+ (or ?- ?. ?_ ";" ?( ?) ?/ ?: alnum)))
                     (and "10.1002/" (+ (not space)) word-boundary))))
  "DOI Regular expression.

Based on https://www.crossref.org/blog/dois-and-matching-regular-expressions/.")

(defun retraction-viewer-doi-at-point ()
  "Get DOI at point."
  (save-match-data
    (when (looking-at retraction-viewer-doi-regexp)
      (match-string-no-properties 1))))

(defun retraction-viewer-current-doi ()
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

;; TODO: Get based on current citation in LaTeX?

;; TODO: Get based on current citation in Org Mode?


;;; Formatting Retraction Notices

(defun retraction-viewer--format-update-nature (notice)
  "Get update-nature from NOTICE."
  (alist-get 'update-nature notice))

(defun retraction-viewer--format-update-date (notice)
  "Get update-date from NOTICE."
  (alist-get 'update-date notice))

(defun retraction-viewer--format-target-doi (notice)
  "Get target-doi from NOTICE."
  (alist-get 'target-doi notice))

(defun retraction-viewer--format-notes (notice)
  "Get notes from NOTICE."
  (alist-get 'notes notice))

(defun retraction-viewer--format-asserted-by (notice)
  "Get asserted-by from NOTICE."
  (alist-get 'asserted-by notice))

(defun retraction-viewer--format-reasons (notice)
  "Get reasons from NOTICE."
  (mapconcat #'identity (alist-get 'reasons notice) ", "))

(defun retraction-viewer--format-urls (notice)
  "Get urls from NOTICE."
  (mapconcat #'identity (alist-get 'urls notice) ", "))

(defun retraction-viewer--format-doi (notice)
  "Get DOI from NOTICE."
  (substring (alist-get 'target-doi notice) 16))

(defun retraction-viewer-format-notice (notice &optional format)
  "Format retraction NOTICE data based on FORMAT.

Use `retraction-viewer-notice-format' if FORMAT is nil.

For available keys, see `retraction-viewer-format-spec'."
  (let* ((format (or format retraction-viewer-notice-format))
         (format-specs (mapcar (lambda (spec)
                                 (cons (car spec)
                                       (if (fboundp (cdr spec))
                                           (funcall (cdr spec) notice)
                                         (symbol-value (cdr spec)))))
                               (cl-remove-if-not (lambda (spec)
                                                   (string-match-p (format "%%%c" (car spec)) format))
                                                 retraction-viewer-format-spec))))
    (format-spec format format-specs)))


;;; Display Methods

;; TODO: Implement eldoc provider
;; add retraction-viewer-eldoc-function to eldoc-documentation-functions buffer-local
;; Implement a mode?

(with-eval-after-load 'universal-sidecar

  (defcustom retraction-viewer-sidecar-modes '(bibtex-mode ebib-entry-mode ebib-index-mode)
    "Which modes should the retraction viewer section be enabled in?"
    :type '(repeat (function :tag "Mode"))
    :group 'universal-sidecar
    :group 'retraction-viewer)

  (universal-sidecar-define-section retraction-viewer-section ((format-string retraction-viewer-notice-format)
                                                               (prepend-bullet ?-))
                                    (:predicate (apply #'derived-mode-p retraction-viewer-sidecar-modes))
    "Show retraction status of the current bibliographic item.

The bullet prepended to each notice message is specified by
PREPEND-BULLET (default -), and the retraction notice format can
be overridden by specifying FORMAT-STRING."
    (when-let ((doi (with-current-buffer buffer (retraction-viewer-current-doi)))
               (retraction-data (retraction-viewer-doi-status doi))
               (prefix (format " %c " prepend-bullet))
               (format-string (if (string-prefix-p prefix format-string)
                                  format-string
                                (concat prefix format-string))))
      (universal-sidecar-insert-section retraction-viewer-section (format "Retraction Notice for %s:" doi)
        (insert (universal-sidecar-fontify-as org-mode ((org-fold-core-style 'overlays))
                  (mapconcat (lambda (entry)
                               (retraction-viewer-format-notice entry format-string))
                             retraction-data
                             "\n")))))))

;; TODO: Other ways to display?

(provide 'retraction-viewer)

;;; retraction-viewer.el ends here
