.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Please cite as: ")
  packageStartupMessage("Data Zoom (2023). Data Zoom: Simplifying Access To Brazilian Microdata.
https://datazoom.com.br/en/")

  packageStartupMessage("\nOr if you prefer a BibTeX entry: ")
  packageStartupMessage("@Unpublished{DataZoom2023,
	     author = {Data Zoom},
	     title = {Data Zoom: Simplifying Access To Brazilian Microdata},
	     url = {https://datazoom.com.br/en/},
	     year = {2023},
}")
}
