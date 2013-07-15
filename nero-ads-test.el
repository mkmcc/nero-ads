(defun collapse-whitespace (s)
  "Convert all adjacent whitespace characters to a single space."
  (replace-regexp-in-string "[ \t\n\r]+" " " s))

(defun ads-assert (test name)
  (if test
      t
    (message "nero-ads: %s failed." name)
    nil))

(let ((test-abs "http://adsabs.harvard.edu/abs/2012MNRAS.419.3319M")
      (test-bib "http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=2012MNRAS.419.3319M&data_type=BIBTEX&db_key=AST&nocookieset=1")
      (test-label "McCourt2012")
      (test-bib-entry "@ARTICLE{2012MNRAS.419.3319M,
   author = {{McCourt}, M. and {Sharma}, P. and {Quataert}, E. and {Parrish}, I.~J.
        },
    title = \"{Thermal instability in gravitationally stratified plasmas: implications for multiphase structure in clusters and galaxy haloes}\",
  journal = {\\mnras},
archivePrefix = \"arXiv\",
   eprint = {1105.2563},
 primaryClass = \"astro-ph.CO\",
 keywords = {galaxies: clusters: intracluster medium, galaxies: evolution, galaxies: haloes},
     year = 2012,
    month = feb,
   volume = 419,
    pages = {3319-3337},
      doi = {10.1111/j.1365-2966.2011.19972.x},
   adsurl = {http://adsabs.harvard.edu/abs/2012MNRAS.419.3319M},
  adsnote = {Provided by the SAO/NASA Astrophysics Data System}
}")
      (test-bib-entry-alt "@ARTICLE{McCourt2012,
   author = {{McCourt}, M. and {Sharma}, P. and {Quataert}, E. and {Parrish}, I.~J.
        },
    title = \"{Thermal instability in gravitationally stratified plasmas: implications for multiphase structure in clusters and galaxy haloes}\",
  journal = {\\mnras},
archivePrefix = \"arXiv\",
   eprint = {1105.2563},
 primaryClass = \"astro-ph.CO\",
 keywords = {galaxies: clusters: intracluster medium, galaxies: evolution, galaxies: haloes},
     year = 2012,
    month = feb,
   volume = 419,
    pages = {3319-3337},
      doi = {10.1111/j.1365-2966.2011.19972.x},
   adsurl = {http://adsabs.harvard.edu/abs/2012MNRAS.419.3319M},
  adsnote = {Provided by the SAO/NASA Astrophysics Data System}
}"))

  (if (and
       (ads-assert
        (string-equal (ads/absurl-to-biburl test-abs) test-bib)
        "correct absurl-to-biburl")
       (ads-assert
        (eq (ads/absurl-to-biburl "http://www.google.com") nil)
        "incorrect absurl-to-biburl")
       (ads-assert
        (string-equal
         (collapse-whitespace (ads/biburl-to-bib test-bib))
         (collapse-whitespace test-bib-entry))
        "correct biburl-to-bib without label replacement")
       (ads-assert
        (string-equal
         (collapse-whitespace (ads/biburl-to-bib test-bib test-label))
         (collapse-whitespace test-bib-entry-alt))
        "correct biburl-to-bib with label replacement")
       (ads-assert
        (eq (ads/biburl-to-bib "http://www.google.com") nil)
        "incorrect biburl-to-bib"))
      (message "nero-ads: all tests succeeded.")
    (message "nero-ads: tests failed.")))
