## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 6 notes ✖

There are four distinct notes across the platforms:

  * Our maintainer has changed
  
  * checking for non-standard things in the check directory ... NOTE  
  Found the following files/directories:  
  ''NULL''
    
  * checking for detritus in the temp directory ... NOTE  
  Found the following files/directories:  
  'lastMiKTeXException'
  
  * checking HTML version of manual ... NOTE  
  Skipping checking HTML validation: no command 'tidy' found
  
Seem to be common and harmless, not sure of the cause.
  
  * Found the following (possibly) invalid URLs:  
    URL: https://dadosabertos.aneel.gov.br/dataset/relacao-de-empreendimentos-de-geracao-distribuida  
      From: inst/doc/ANEEL.html  
            README.md  
      Status: 403  
      Message: Forbidden
      
The URL works for us.  

