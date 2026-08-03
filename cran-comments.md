
  * Some functionalities may be unstable depending on the provider. 

  * On first use, the package downloads a small CSV manifest of source URLs
    from the package's own GitHub repository. If the network is unavailable
    the package silently uses the identical copy shipped in `inst/extdata`,
    so all functionality works offline. No network access occurs at load
    time, in examples, in tests, or during vignette building. The download
    writes only to `tempfile()` and restores all modified `options()` on
    exit.

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 notes✖

There is one note across the platforms:

  * Our maintainer has changed.
  
  * Some URLs to Brazilian government sources (e.g. www.ibge.gov.br)
    are flagged as invalid, but they work for us.

