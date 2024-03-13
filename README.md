[![REUSE status](https://api.reuse.software/badge/git.sr.ht/~swflint/retraction-viewer)](https://api.reuse.software/info/git.sr.ht/~swflint/retraction-viewer)
[![MELPA](https://melpa.org/packages/retraction-viewer-badge.svg)](https://melpa.org/#/retraction-viewer)

# Retraction Viewer

This emacs package provides a way to show retraction information for citations and citation data at point.
This is done using the [Crossref REST API](https://www.crossref.org/documentation/retrieve-metadata/rest-api/) (experimental version until the feature is available on the regular version).
At the moment, it explicitly supports detection of DOI from [ebib](http://joostkremers.github.io/ebib/) as well as `bibtex-mode`.

It will be possible to show retraction information as a [Universal Sidecar](https://git.sr.ht/~swflint/emacs-universal-sidecar) section, as well as through `eldoc`.

## Configuration

### Sidecar Section

TODO

### Eldoc Configuration

TODO

### DOI getters

## Use as a library

TODO

`retraction-viewer-get-current-doi`

`retraction-viewer-doi-status`
