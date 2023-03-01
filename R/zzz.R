.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Please cite as: ")
  packageStartupMessage("Data Zoom (2023). Data Zoom: Simplifying Access To Brazilian Microdata.
https://www.econ.puc-rio.br/datazoom/english/index.html")

  packageStartupMessage("\nOr if you prefer a BibTeX entry: ")
  packageStartupMessage("@Unpublished{DataZoom2023,
	     author = {Data Zoom},
	     title = {Data Zoom: Simplifying Access To Brazilian Microdata},
	     url = {https://www.econ.puc-rio.br/datazoom/english/index.html},
	     year = {2023},
}")
}
