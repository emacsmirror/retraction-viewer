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
;; This Emacs package provides a way to show retraction information
;; for citations and citation data at point.  This is done using the
;; Crossref REST API
;; (https://www.crossref.org/documentation/retrieve-metadata/rest-api/)
;; (experimental version until the feature is available on the regular
;; version).  At the moment, it explicitly supports detection of DOI
;; from `ebib' (http://joostkremers.github.io/ebib/) as well as
;; `bibtex-mode'.
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
;;;;; Sidecar Section
;;
;; If the universal sidecar
;; (https://git.sr.ht/~swflint/emacs-universal-sidecar) package is
;; loaded (before or after this package), a sidecar section is made
;; available.  It can be used, as any other, by adding it to the
;; `universal-sidecar-sections' list, such as shown below.
;;
;;    (add-to-list 'universal-sidecar-sections 'retraction-viewer-section)
;;
;; There are three options for configuration.  First is the
;; customizable variable, `retraction-viewer-sidecar-modes', which
;; specifies the modes in which the sidecar is applicable (its default
;; is bibtex mode and ebib-related modes).  Second is the notice
;; format string keyword argument, `:format-string', which is a format
;; string as described above.  Finally, the bullet character can be
;; modified with the `:prepend-bullet' argument, which should be a
;; valid org-mode bullet (useful for users of `org-bullets' or
;; `org-superstar').
;;
;;;;; Eldoc Configuration
;;
;; Forthcoming.
;;
;;;;; DOI Getters
;;
;; Finally, it's possible to configure how the "current DOI" is
;; detected using the `retraction-viewer-get-doi-functions' hook.  The
;; functions are evaluated until one returns non-nil.  By default, it
;; will get the DOI from ebib if called in an ebib buffer
;; (`retraction-viewer-get-ebib-doi'), from the bibtex entry-at-point
;; if within a bibtex buffer (`retraction-viewer-get-bibtex-doi'), or
;; if point is on a DOI (see `retraction-viewer-doi-regexp').
;; Additional functions can be written to select a current DOI, and
;; should operate by: a) not adjusting match data; b) not adjust
;; point/mark; c) not adjust narrowing, and d) fail early (i.e.,
;; return nil ASAP).
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

;; TODO: Add customizable timeouts (connect-timeout and timeout), consider tuning?

(defcustom retraction-viewer-get-doi-functions (list #'retraction-viewer-get-ebib-doi
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

(defun retraction-viewer-get-bibtex-doi ()
  "Get DOI from current BibTeX entry."
  (when (derived-mode-p 'bibtex-mode)
    (save-excursion
      (save-match-data
        (bibtex-text-in-field "doi")))))

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
                                       (or (if (fboundp (cdr spec))
                                               (funcall (cdr spec) notice)
                                             (symbol-value (cdr spec)))
                                           "")))
                               (cl-remove-if-not (lambda (spec)
                                                   (string-match-p (format "%%%c" (car spec)) format))
                                                 retraction-viewer-format-spec))))
    (format-spec format format-specs)))


;;; Eldoc Support

;; TODO: Implement eldoc provider
;; add retraction-viewer-eldoc-function to eldoc-documentation-functions buffer-local
;; Implement a mode?


;;; Universal Sidecar Section

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

(provide 'retraction-viewer)

;;; retraction-viewer.el ends here
