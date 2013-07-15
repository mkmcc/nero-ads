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
;;     retrieves the bibtex entry.  This command is bound to 'z'.

;; Example usage: insert the bibtex entry for Quataert (2008) into
;; paper.bib
;;
;;   M-x nero-query-nasa-ads RET ^Quataert 2008 RET
;;   z 7 RET
;;   C-x b paper.bib
;;   C-y

;;; Code:

(require 'nero)
(defvar nero-history nil)               ; not sure why this is needed...

;; prompt for a search string and construct an ADS url
(nero-defelvis "Nasa ADS"
  "http://adsabs.harvard.edu/cgi-bin/nph-basic_connect?qsearch="
  "%20"
  "&version=1")


;; functions to automatically find ADS bibtex entries
;;
(defvar ads-scratch-buffer "*ADS scratch*")

(defun ads/absurl-to-biburl (abs-url)
  "Take the URL of an ADS abstract page and return a URL for the
corresponding bibtex entry.  Return nil if not found."
  (with-current-buffer ads-scratch-buffer
    (erase-buffer)
    ; use lynx -dump to parse the html, find links, etc.
    (call-process "lynx" nil ads-scratch-buffer nil "-dump" abs-url)
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
  (with-current-buffer ads-scratch-buffer
    (erase-buffer)
    ; lynx -source doesn't process the text at all
    (call-process "lynx" nil ads-scratch-buffer nil "-source" bib-url)
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

(defun nero-slurp-bibtex (&optional link-number new-label)
  ""
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

(eval-after-load 'nero
  '(define-key nero-mode-map (kbd "z") 'nero-slurp-bibtex))


(provide 'nero-ads)
;;; nero-ads.el ends here
