;;; retraction-viewer.el --- View retraction information for current citation  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1") (plz "0.9.1"))
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
;; This Emacs package provides a way to show retraction information
;; for citations and citation data at point.  This is done using the
;; Crossref REST API
;; (https://www.crossref.org/documentation/retrieve-metadata/rest-api/).
;; At the moment, it explicitly supports detection of DOI from `ebib'
;; (http://joostkremers.github.io/ebib/) as well as `bibtex-mode'.
;;
;; It is possible to show retraction information as a Universal
;; Sidecar Section
;; (https://git.sr.ht/~swflint/emacs-universal-sidecar), and support
;; for `eldoc' is forthcoming.
;;
;;;; Configuration
;;
;; For general use, there is one major variable that must be
;; customized.  `retraction-viewer-crossref-email' should be set to
;; your email address (or an email address that will be checked, see
;; https://github.com/CrossRef/rest-api-doc#good-manners--more-reliable-service).
;; In particular, it may be helpful to set it to `user-mail-address',
;; as follows.
;;
;;     (setopt retraction-viewer-crossref-email user-mail-address)
;;
;;;;; Notice Formatting
;;
;; It is possible to customize how notices are formatted using the
;; `retraction-viewer-notice-format' variable.  The % escapes are
;; defined in `retraction-viewer-format-spec', which can be customized
;; as well.
;;
;; The `retraction-viewer-format-spec' is an alist of (unique)
;; characters to either variable names or functions taking a
;; retraction notice message.  In either case, they should return a
;; string, or nil (which will become the empty string).  Retraction
;; notice data is a direct translation from the JSON output of the
;; CrossRef REST API.  As it is at present using the experimental
;; version of the API keys are subject to change.
;;
;;;;; Eldoc Configuration
;;
;; Retraction notices can also be showed using `eldoc', by enabling
;; both `eldoc-mode' and `retraction-viewer-eldoc-mode'.  Note, this
;; operates asynchronously, and will format notices using
;; `retraction-viewer-notice-format'.  Additionally, collecting
;; retraction data is subject to collecting a DOI (see
;; `retraction-viewer-get-doi-functions' described below).
;;
;;;;; DOI Getters
;;
;; Finally, it's possible to configure how the "current DOI" is
;; detected using the `retraction-viewer-get-doi-functions' hook.  The
;; functions are evaluated until one returns non-nil.  By default, it
;; will get the DOI from ebib if called in an ebib buffer
;; (`retraction-viewer-get-ebib-doi'), from the bibtex entry-at-point
;; if within a bibtex buffer (`retraction-viewer-get-bibtex-doi'),
;; from the currently shown elfeed entry if in an `elfeed-show' buffer
;; (`retraction-viewer-get-elfeed-doi'), or if point is on a DOI (see
;; `retraction-viewer-doi-regexp').  Additional functions can be
;; written to select a current DOI, and should operate by: a) not
;; adjusting match data; b) not adjust point/mark; c) not adjust
;; narrowing, and d) fail early (i.e., return nil ASAP).
;;
;;;; Use as a Library
;;
;; Additionally, `retraction-viewer' is intended to be a way for other
;; packages to get retraction information and related data easily.  To
;; this end, there are three categories of functions it provides: DOI
;; detection, retraction status, and notice formatting.
;;
;; DOIs can be detected using `retraction-viewer-current-doi' to
;; determine if there is a DOI in the current context (whatever that
;; may be given mode, etc.), for more information about how this can
;; be extended, see above.  Additionally, the
;; `retraction-viewer-doi-at-point' can be used directly to determine
;; if there is a DOI at point (using `retraction-viewer-doi-regexp').
;;
;; The retraction status of a DOI can be determined with
;; `retraction-viewer-doi-status', which will return a list of alists
;; describing any retraction notices found in the RetractionWatch
;; database.  These alists are at present subject to change.
;; Additionally, a callback, taking the status record can be passed as
;; an optional second argument; for an example of use, see
;; `retraction-viewer-eldoc-function'.
;;
;; Finally, retraction notices can be formatted easily using a
;; format-string like construct using
;; `retraction-viewer-format-notice', (see above section, "Notice
;; Formatting" for more information).
;;
;;;; Errors and Patches
;;
;; If you find an error, or have a patch to improve this package (or
;; are able to add additional DOI getters), please send an email to
;; ~swflint/emacs-utilities@lists.sr.ht.



;;; Code:
(require 'plz)
(require 'iso8601)
(require 'json)
(require 'bibtex)


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

(defcustom retraction-viewer-get-doi-functions (list #'retraction-viewer-get-ebib-doi
                                                     #'retraction-viewer-get-bibtex-doi
                                                     #'retraction-viewer-get-elfeed-doi
                                                     #'retraction-viewer-doi-at-point)
  "How should a DOI be gotten?

This is a list of functions, run until one returns non-nil."
  :group 'retraction-viewer
  :type 'hook)

(defcustom retraction-viewer-format-spec
  '((?d . retraction-viewer--format-date)
    (?L . retraction-viewer--format-date-local)
    (?t . retraction-viewer--format-type)
    (?s . retraction-viewer--format-source)
    (?l . retraction-viewer--format-label)
    (?D . retraction-viewer--format-doi)
    (?U . retraction-viewer--format-notice-url))
  "Metacharacters for formatting retraction notices.

Keys should be unique characters, and values should be either
references to variables or named functions which take a single
argument, the retraction notice data structure (alist)."
  :group 'retraction-viewer
  :type '(alist :key-type (character :tag "Format Character")
                :value-type (sexp :tag "Getter")))

(defcustom retraction-viewer-notice-format "%l (%L): %s (see also [[%U][%D]])."
  "Default format for retraction notices.

See also `retraction-viewer-format-spec' for available keys."
  :group 'retraction-viewer
  :type 'string)

(defcustom retraction-viewer-date-format "%d %B %Y, %H:%M"
  "Format for showing dates.

See `format-time-string'."
  :group 'retraction-viewer
  :type 'string)


;;; Utility Functions

(defun retraction-viewer--format-url (doi)
  "Format data URL for DOI.

Note, `retraction-viewer-crossref-email' must be set."
  (if (null retraction-viewer-crossref-email)
      (display-warning 'retraction-viewer "Please set `retraction-viewer-crossref-email' to a string value" :error)
    (format-spec "https://api.crossref.org/works/%d?mailto=%m"
                 `((?d . ,doi)
                   (?m . ,retraction-viewer-crossref-email)))))

(define-hash-table-test 'retraction-watch-string=
                        'string= 'sxhash-equal)

(defvar retraction-viewer--cached-retraction-status
  (make-hash-table :test 'retraction-watch-string=)
  "Cached results for retraction viewer.")


;;; Get DOI Retraction Status

(defconst retraction-viewer--applicable-message-types
  (list "expression_of_concern"
        "retraction"
        "correction"))

(defun retraction-viewer--process-json (callback doi data)
  "Process JSON DATA, calling CALLBACK if not nil.

Save data to DOI."
  (when-let* ((message (alist-get 'message data))
              (updates (cl-map 'list #'identity (alist-get 'updated-by message)))
              (retraction-messages (cl-remove-if-not (lambda (entry)
                                                       (cl-member (alist-get 'type entry)
                                                                  retraction-viewer--applicable-message-types
                                                                  :test #'string=))
                                                     updates)))
    (message "%S" retraction-messages)
    (funcall (if callback callback #'identity)
             (puthash doi retraction-messages
                      retraction-viewer--cached-retraction-status))))

(defun retraction-viewer-doi-status (doi &optional callback)
  "Get the retraction status of DOI, optionally providing information to CALLBACK."

  (if-let ((hash-entry (gethash doi retraction-viewer--cached-retraction-status)))
      (if callback
          (funcall callback hash-entry)
        hash-entry)
    (let ((url (retraction-viewer--format-url doi))
          (kill-buffer-hook nil))
      (condition-case-unless-debug err
          (if callback
              (plz 'get url
                :as #'json-read
                :then (apply-partially #'retraction-viewer--process-json callback doi)
                :else #'ignore
                :noquery t)
            (let ((data (plz 'get url
                          :as #'json-read
                          :noquery t)))
              (retraction-viewer--process-json nil doi data)))
        (t nil)))))


;;; Get current DOI

(defconst retraction-viewer-doi-regexp
  (rx (group-n 1 (or (and "10." (>= 4 digit) "/" (+ (or ?- ?. ?_ ";" ?\( ?\) ?/ ?: alnum)))
                     (and "10.1002/" (+ (not space)) word-boundary))))
  "DOI Regular expression.

Based on https://www.crossref.org/blog/dois-and-matching-regular-expressions/.")

(defun retraction-viewer-doi-at-point ()
  "Get DOI at point."
  (save-excursion
    (save-match-data
      (when (looking-at retraction-viewer-doi-regexp)
        (match-string-no-properties 1)))))

(defun retraction-viewer-current-doi ()
  "Get the current DOI, using `retraction-viewer-doi-functions'."
  (run-hook-with-args-until-success 'retraction-viewer-get-doi-functions))

(declare-function ebib-get-field-value "ebib-utils.el")
(declare-function ebib--get-key-at-point "ebib.el")
(defvar ebib--cur-db)
(defun retraction-viewer-get-ebib-doi ()
  "Get DOI from current Ebib entry."
  (when (and (featurep 'ebib)
             (derived-mode-p 'ebib-entry-mode 'ebib-index-mode))
    (ebib-get-field-value "doi" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced)))

(declare-function elfeed-show-entry "elfeed-show.el")
(defvar elfeed-show-entry)
(defun retraction-viewer-get-elfeed-doi ()
  "Get DOI from current elfeed entry."
  (when (and (featurep 'elfeed)
             (derived-mode-p 'elfeed-show-mode))
    (when-let ((url (elfeed-entry-link elfeed-show-entry))
               (matchp (string-match retraction-viewer-doi-regexp url)))
      (match-string 1 url))))

(defun retraction-viewer-get-bibtex-doi ()
  "Get DOI from current BibTeX entry."
  (when (derived-mode-p 'bibtex-mode)
    (save-excursion
      (save-match-data
        (bibtex-text-in-field "doi")))))

;; TODO: Get based on current citation in LaTeX?

;; TODO: Get based on current citation in Org Mode?


;;; Formatting Retraction Notices

;; type
;; label
;; source
;; update DOI (DOI)
;; Updated Time (updated.timestamp)

(defun retraction-viewer--format-date (notice)
  "Get date from NOTICE."
  (alist-get 'date-time (alist-get 'updated notice)))

(defun retraction-viewer--format-date-local (notice)
  "Get and format date/time from NOTICE.

See also `retraction-viewer-date-format'"
  (format-time-string retraction-viewer-date-format
                      (encode-time
                       (iso8601-parse
                        (alist-get 'date-time
                                   (alist-get 'updated notice))))))

(defun retraction-viewer--format-type (notice)
  "Get notice type from NOTICE."
  (alist-get 'type notice))

(defun retraction-viewer--format-label (notice)
  "Get label from NOTICE."
  (alist-get 'label notice))

(defun retraction-viewer--format-source (notice)
  "Get source from NOTICE."
  (alist-get 'source notice))

(defun retraction-viewer--format-doi (notice)
  "Get DOI from NOTICE."
  (alist-get 'DOI notice))

(defun retraction-viewer--format-notice-url (notice)
  "Get DOI as URL from NOTICE."
  (format "https://doi.org/%s" (alist-get 'DOI notice)))

(defun retraction-viewer-format-notice (notice &optional format)
  "Format retraction NOTICE data based on FORMAT.

Use `retraction-viewer-notice-format' if FORMAT is nil.

For available keys, see `retraction-viewer-format-spec'."
  (let* ((format (or format retraction-viewer-notice-format))
         (format-specs (mapcar (lambda (spec)
                                 (cons (car spec)
                                       (or (if (fboundp (cdr spec))
                                               (funcall (cdr spec) notice)
                                             (symbol-value (cdr spec)))
                                           "")))
                               (cl-remove-if-not (lambda (spec)
                                                   (string-match-p (format "%%%c" (car spec)) format))
                                                 retraction-viewer-format-spec))))
    (format-spec format format-specs)))

(defun retraction-viewer-format-notices (notices &optional format)
  "Format retraction NOTICES data based on FORMAT.

See also `retraction-viewer-format-notice'."
  (string-join (mapcar (lambda (notice) (retraction-viewer-format-notice notice format))
                       notices)
               "\n"))


;;; Eldoc Support

(defun retraction-viewer-eldoc-function (callback)
  "Call CALLBACK if there is a retraction notice for the current DOI."
  (when-let ((doi (retraction-viewer-current-doi)))
    (retraction-viewer-doi-status doi
                                  (lambda (retraction-data)
                                    (funcall callback (mapconcat #'retraction-viewer-format-notice retraction-data "\n"))))
    :async-call))

(define-minor-mode retraction-viewer-eldoc-mode
  "Show retraction data for the current bibliographic item using `eldoc'."
  :lighter " RV"
  (if retraction-viewer-eldoc-mode
      (add-hook 'eldoc-documentation-functions #'retraction-viewer-eldoc-function :anywhere :local)
    (remove-hook 'eldoc-documentation-functions #'retraction-viewer-eldoc-function :local)))

(provide 'retraction-viewer)

;;; retraction-viewer.el ends here
