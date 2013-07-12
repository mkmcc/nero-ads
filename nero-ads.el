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

;; prompt for a search string and construct an ADS url
(nero-defelvis "Nasa ADS"
  "http://adsabs.harvard.edu/cgi-bin/nph-basic_connect?qsearch="
  "%20"
  "&version=1")

(defun nero-ads-copy-bibtex ()
  "Extract the bibtex definition from a nero buffer and save it
to the kill ring.

This function only works inside a buffer displaying the bibtex
entry -- it's just a helper function for `nero-slurp-bibtex'"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\\(@\\w+\\)")
    (let ((front (match-string-no-properties 1))
          (bpoint (point)))
      (forward-sexp 1)
      (kill-new (concat
                 front
                 (buffer-substring-no-properties bpoint (point)))))))

(defun nero-slurp-bibtex (&optional link-number)
  "Read a link number and save the corresponding bibtex entry to the kill-ring.

The link you provide should point to an ADS \"abstract\" page.
So you'd use this from, e.g. a page listing search results.  This
code is a bit ugly, but the functionality is awesome."
  (interactive (list (read-string "Slurp bibtex from Link Number: ")))
  (let ((url (nero-follow-link-internal link-number 'return-link nil t)))
    ;; use nero to browse this url since it parses links automatically
    (nero-browse-url url nil nil nil
       (lambda ()
         (cond
          ((search-forward "Bibtex" nil t)
           (nero-move-to-previous-link)
           (let ((biburl (nero-down 'return-link)))
             ;; use url-retrieve here since we don't want any
             ;; processing of the file
             (url-retrieve biburl
                           (lambda (status) (nero-ads-copy-bibtex))))
           (message "Copied bibtex entry to kill-ring."))
          (t
           (message "Couldn't find bibtex entry."))))
       t)))

(eval-after-load 'nero
  '(define-key nero-mode-map (kbd "z") 'nero-slurp-bibtex))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'nero-ads)
