#' Load Interface for Google EE
#'
#'Execute the ee_Initialize command. You will be directed to your Google Earth
#' Engine and get access to it using rgee package
#'
#' @return
#' @export
#'
#' @examples
initialize_ee = function(){


  ## Install Google Earth Engine for the first time

  # install.packages('sf')
  # install.packages('mapview')
  # install.packages('remotes')
  #
  if (!require("rgee")) {
    remotes::install_github("rgee")
    rgee::ee_install()
  }

  ## Initialize EE

  #rgee::ee_check()
  rgee::ee_Initialize()
}
