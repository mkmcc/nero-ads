(require 's)

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


  (unless (string-equal (ads/absurl-to-biburl test-abs) test-bib)
    (message "test 1 failed."))
  (unless (eq (ads/absurl-to-biburl "http://www.google.com") nil)
    (message "test 2 failed."))

  (unless (string-equal
           (s-collapse-whitespace (ads/biburl-to-bib test-bib))
           (s-collapse-whitespace test-bib-entry))
    (message "test 3 failed."))
  (unless (string-equal
           (s-collapse-whitespace (ads/biburl-to-bib test-bib test-label))
           (s-collapse-whitespace test-bib-entry-alt))
    (message "test 4 failed."))
  (unless (eq (ads/biburl-to-bib "http://www.google.com") nil)
    (message "test 5 failed.")))
