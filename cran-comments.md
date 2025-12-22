## Test environments
- R-CMD-check macos-latest (release)
- R-CMD-check windows-latest (release)
- R-CMD-check ubuntu-latest (devel)
- R-CMD-check ubuntu-latest (release)
- R-CMD-check ubuntu-latest (oldrel-1)

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 3 notes ✖

There are two distinct notes across the platforms:

  * Our maintainer has changed.
  
  * Found the following (possibly) invalid URLs:
    URL: http://terrabrasilis.dpi.inpe.br/downloads/ (moved to https://terrabrasilis.dpi.inpe.br/downloads/)
      From: inst/doc/PRODES.html
            README.md
      Status: 200
      Message: OK
    URL: http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37 (moved to https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37&id=37)
      From: inst/doc/BACI.html
            README.md
      Status: 200
      Message: OK
      
Both URL work for us.  
