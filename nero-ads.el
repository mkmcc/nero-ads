;;; nero-ads.el --- utilities to query NASA ADS from within emacs

;; Copyright (c) 2013 Mike McCourt (mkmcc@berkeley.edu)
;;

;;; Commentary:

;; Extends the incredibly useful nero.el (code.google.com/p/nero-el)
;; to interact with NASA's Astrophysics Data System (ADS).
;;
;; Provides two interactive functions:
;;
;; 1. `nero-query-nasa-ads': prompts for a search string and passes
;;     the query onto ADS automatically.  shows the results in the
;;     *Nero* buffer.
;;
;; 2. `nero-slurp-bibtex': prompts for a link number and automatically
;;     retrieves the bibtex entry.  also prompts for an optional
;;     bibtex label to replace the default NASA one.  This command is
;;     bound to 'z'.

;; Notes:
;;
;; 1. `nero-slurp-bibtex' calls lynx directly, rather than going
;;    through the nero interface.  This means it won't affect the nero
;;    "time-scape", which I think is a good thing.
;;
;; 2. Since `nero-slurp-bibtex' calls lynx synchronously, you have to
;;    wait for the pages to download before you can continue working.
;;    This is different from the way nero works, but it fits my usage
;;    pattern at least: when I call `nero-slurp-bibtex', I usually
;;    want to yank the result into my bib database immediately.  Also,
;;    the NASA ADS system is fast enough that waiting for it hasn't
;;    really annoyed me yet.

;; TODO:
;;
;; 1. make nero-slurp-bibtex guess new-label from context?  maybe that
;;    functionality really belongs in ads/biburl-to-bib...

;; Example usage:
;;
;; 1. insert the bibtex entry for Quataert (2008) into paper.bib
;;
;;    M-x nero-query-nasa-ads RET ^Quataert 2008 RET
;;    z 7 RET Quataert2008 RET
;;    C-x b paper.bib
;;    C-y

;;; Code:

(require 'nero)
(defvar nero-history nil)      ; I think this is a bug in nero.el (??)

;; prompt for a search string and construct an ADS url
;;;###autoload
(eval-after-load 'nero
  '(nero-defelvis "Nasa ADS"
     "http://adsabs.harvard.edu/cgi-bin/nph-basic_connect?qsearch="
     "%20"
     "&version=1"))


;; functions to automatically find ADS bibtex entries
;;
(defun ads/absurl-to-biburl (abs-url)
  "Take the URL of an ADS abstract page and return a URL for the
corresponding bibtex entry.  Return nil if not found."
  (with-temp-buffer
    ; use lynx -dump to parse the html, find links, etc.
    (call-process "lynx" nil t nil "-dump" abs-url)
    (goto-char (point-min))
    ; look for, e.g. "[25]Bibtex entry for this abstract"
    (when (re-search-forward "\\[\\([0-9]+\\)\\]Bibtex" nil t)
      ; look for, e.g. " 25. http://..."
      (let ((bib-link-regexp
             (concat "^\\s-*" (match-string-no-properties 1)
                     "\\.\\s-*\\(.+\\)$")))
        (re-search-forward "^References$")
        (re-search-forward bib-link-regexp)
        (match-string-no-properties 1)))))

(defun ads/biburl-to-bib (bib-url &optional new-label)
  "Take the URL for an ADS bibtex entry and return the entry as a
string.  Optionally, replace the default (and useless) ADS label
with the argument NEW-LABEL."
  (with-temp-buffer
    ; lynx -source doesn't process the text at all
    (call-process "lynx" nil t nil "-source" bib-url)
    ; first, look for a bibtex definition and replace the label if
    ; appropriate.
    (goto-char (point-min))
    (when (re-search-forward "@\\sw+{\\([^,]+\\)," nil t)
      (when (and new-label (not (string-equal new-label "")))
        (replace-match new-label t t nil 1))
      ; next, find the definition and return it.  use the nifty
      ; function `forward-sexp' to navigate to the end.
      (goto-char (point-min))
      (re-search-forward "@\\sw+")
      (let ((bpoint (point)))
        (forward-sexp)
        (concat (match-string-no-properties 0)
                (buffer-substring bpoint (point)))))))

;;;###autoload
(defun nero-slurp-bibtex (&optional link-number new-label)
  "Automatically find the bibtex entry for an abstract in the
NASA ADS database.

This function is rather specific -- it presumes you've used
`nero-query-nasa-ads' to search ADS for an abstract.  Then, you
can call this function from the *Nero* buffer.  It will prompt
for the number in front of the abstract you want, then will find
the bibtex entry and save it to the kill ring.

The functions `ads/absurl-to-biburl' and `ads/biburl-to-bib' are
more general.  Specifically, they don't depend on nero at all."
  (interactive (list (read-string "Slurp bibtex from Link Number: ")
                     (read-string "New label: ")))
  (let* ((abs-url (nero-follow-link-internal link-number 'return-link nil t))
         (bib-url (ads/absurl-to-biburl abs-url)))
    (cond
     ((eq bib-url nil)
      (message "Couldn't find link to bibtex entry."))
     (t
      (kill-new (ads/biburl-to-bib bib-url new-label))
      (message "Saved bibtex entry to kill-ring.")))))

;;;###autoload
(eval-after-load 'nero
  '(define-key nero-mode-map (kbd "z") 'nero-slurp-bibtex))

(provide 'nero-ads)

;;; nero-ads.el ends here
