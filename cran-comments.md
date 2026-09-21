
  * Some functionalities may be unstable depending on the provider. 

  * On first use, the package downloads a small CSV manifest of source URLs
    from the package's own GitHub repository. If the network is unavailable
    the package silently uses the identical copy shipped in `inst/extdata`,
    so all functionality works offline. No network access occurs at load
    time, in examples, in tests, or during vignette building. The download
    writes only to `tempfile()` and restores all modified `options()` on
    exit.

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

There are two notes, both expected:

  * Version contains large components (this is a development version;
    the CRAN-submitted version will not carry the `.9000` suffix).

  * Several URLs to Brazilian government sources (e.g. sidra.ibge.gov.br,
    www.ibge.gov.br, oc.eco.br, www.inpe.br) are flagged as invalid or
    unreachable by the automated URL check, but work for us in a browser.
    These hosts routinely 403 automated/bot traffic and, in the case of
    www.inpe.br, were unreachable from the machine this check was run on;
    none of them are downloaded by the package itself (only referenced in
    documentation), so this does not affect functionality.

