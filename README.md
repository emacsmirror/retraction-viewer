[![REUSE status](https://api.reuse.software/badge/git.sr.ht/~swflint/retraction-viewer)](https://api.reuse.software/info/git.sr.ht/~swflint/retraction-viewer)
[![Not On MELPA Yet](https://melpa.org/packages/retraction-viewer-badge.svg)](https://melpa.org/#/retraction-viewer)

# Retraction Viewer

This Emacs package provides a way to show retraction information for citations and citation data at point.
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
Retraction notice data is a direct translation from the JSON output of the CrossRef REST API.
As it is at present using the experimental version of the API keys are subject to change.

### Sidecar Section

If the universal sidecar (https://git.sr.ht/~swflint/emacs-universal-sidecar) package is loaded (before or after this package), a sidecar section is made available.
It can be used, as any other, by adding it to the `universal-sidecar-sections` list, such as shown below.

```elisp
(add-to-list 'universal-sidecar-sections 'retraction-viewer-section)
```

There are three options for configuration.
First is the customizable variable, `retraction-viewer-sidecar-modes`, which specifies the modes in which the sidecar is applicable (its default is bibtex mode and ebib-related modes).
Second is the notice format string keyword argument, `:format-string`, which is a format string as described above.
Finally, the bullet character can be modified with the `:prepend-bullet` argument, which should be a valid org-mode bullet (useful for users of `org-bullets` or `org-superstar`).

### Eldoc Configuration

Forthcoming.

### DOI getters

Finally, it's possible to configure how the "current DOI" is detected using the `retraction-viewer-get-doi-functions` hook. The functions are evaluated until one returns non-nil.
By default, it will get the DOI from ebib if called in an ebib buffer (`retraction-viewer-get-ebib-doi`), from the bibtex entry-at-point if within a bibtex buffer (`retraction-viewer-get-bibtex-doi`), or if point is on a DOI (see `retraction-viewer-doi-regexp`).
Additional functions can be written to select a current DOI, and should operate by: a) not adjusting match data; b) not adjust point/mark; c) not adjust narrowing, and d) fail early (i.e., return nil ASAP).

## Use as a library

TODO

## Errors and Patches

If you find an error, or have a patch to improve this package (or are able to add additional DOI getters), please send an email to `~swflint/emacs-utilities@lists.sr.ht`.
