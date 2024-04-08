;;; retraction-viewer-section.el --- Show retraction information in the universal-sidecar  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; Version: 1.0.0
;; Package-Requires: ((retraction-viewer "1.0.2") (universal-sidecar "1.5.1"))
;; Keywords: bib, tex
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
;; Retraction data from `retraction-viewer' may be shown in the
;; `universal-sidecar'
;; (https://git.sr.ht/~swflint/emacs-universal-sidecar) as follows.
;;
;;     (require 'retraction-viewer-section)
;;     (add-to-list 'universal-sidecar-sections 'retraction-viewer-section)
;;
;; There are three main options for configuration.  First is the
;; customizable variable, `retraction-viewer-section-modes', which
;; specifies the modes in which the sidecar is applicable (its default
;; is bibtex mode and ebib-related modes).  Second is the notice
;; format string keyword argument, `:format-string', which is a format
;; string as described above.  Finally, the bullet character can be
;; modified with the `:prepend-bullet' argument, which should be a
;; valid org-mode bullet (useful for users of `org-bullets' or
;; `org-superstar').


;;; Code:

(require 'retraction-viewer)
(require 'universal-sidecar)


;; Customization

(defcustom retraction-viewer-section-modes '(bibtex-mode ebib-entry-mode ebib-index-mode)
  "Which modes should the retraction viewer section be enabled in?"
  :type '(repeat (function :tag "Mode"))
  :group 'universal-sidecar
  :group 'retraction-viewer)


;; Sidecar Implementation

(universal-sidecar-define-section retraction-viewer-section ((format-string retraction-viewer-notice-format)
                                                             (prepend-bullet ?-))
                                  (:predicate (apply #'derived-mode-p retraction-viewer-section-modes))
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
                           "\n"))))))

(provide 'retraction-viewer-section)

;;; retraction-viewer-section.el ends here
