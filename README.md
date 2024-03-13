[![REUSE status](https://api.reuse.software/badge/git.sr.ht/~swflint/retraction-viewer)](https://api.reuse.software/info/git.sr.ht/~swflint/retraction-viewer)
[![Not On MELPA Yet](https://melpa.org/packages/retraction-viewer-badge.svg)](https://melpa.org/#/retraction-viewer)

# Retraction Viewer

This emacs package provides a way to show retraction information for citations and citation data at point.
This is done using the [Crossref REST API](https://www.crossref.org/documentation/retrieve-metadata/rest-api/) (experimental version until the feature is available on the regular version).
At the moment, it explicitly supports detection of DOI from [ebib](http://joostkremers.github.io/ebib/) as well as `bibtex-mode`.

It is possible show retraction information as a [Universal Sidecar Section](https://git.sr.ht/~swflint/emacs-universal-sidecar), and support for `eldoc` is forthcoming.

## Configuration

For general use, there is one major variable that must be customized.
`retraction-viewer-crossref-email` should be set to your email address (or an email address that will be checked, see https://github.com/CrossRef/rest-api-doc#good-manners--more-reliable-service).
In particular, it may be helpful to set it to `user-mail-address`, as follows.

```elisp
(setopt retraction-viewer-crossref-email user-mail-address)
```

### Notice Formatting

It is possible to customize how notices are formatted using the `retraction-viewer-notice-format` variable.
The % escapes are defined in `retraction-viewer-format-spec`, which can be customized as well.

The `retraction-viewer-format-spec` is an alist of (unique) characters to either variable names or functions taking a retraction notice message.
In either case, they should return a string, or nil (which will become the empty string).

### Sidecar Section

### Eldoc Configuration

Forthcoming.

### DOI getters

## Use as a library

TODO
