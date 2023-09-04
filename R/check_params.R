check_params <- function(param, source){

  ###########################
  ## Bind Global Variables ##
  ###########################

  survey <- dataset <- available_time <- available_geo <- . <- NULL

  ###################
  ## Dataset check ##
  ###################

  # extracting the supported datasets from datasets_link

  supp_datasets <- datasets_link() %>%
    dplyr::filter(survey == source) %>%
    dplyr::select(dataset) %>%
    unlist() # vector with all possible datasets

  if (!(param$dataset %in% supp_datasets)) {
    dataset_error <- paste("Dataset", param$dataset, "not supported.",
                           "Dataset must be one of", paste(supp_datasets, collapse = " "))

    stop(dataset_error)
  }

  #######################
  ## Time period check ##
  #######################

  if (!is.null(param$time_period)){

    # constructing vector of supported years

    supp_time_period_str <- datasets_link() %>%
      dplyr::filter(survey == source, dataset == param$dataset) %>%
      dplyr::select(available_time) %>%
      unlist()

    # separating by commas: 2003, 2007, 2014 -> c(2003, 2007, 2014)

    supp_time_period <- supp_time_period_str %>%
      stringr::str_split(",", simplify = TRUE)

    # separating by hyphens: 2003 - 2007 -> 2003:2007

    supp_time_period <- supp_time_period %>%
      stringr::str_replace("-", ":") %>%
      parse(text = .) %>%
      eval()

    if (!all(param$time_period %in% supp_time_period)) {
      time_period_error <- paste("Option time_period must be in", supp_time_period_str)

      stop(time_period_error)
    }

  }

  #####################
  ## Geo level check ##
  #####################

  if (!(is.null(param$geo_level))) {

    # constructing vector of supported geo_levels

    supp_geo_level_str <- datasets_link() %>%
      dplyr::filter(survey == source, dataset == param$dataset) %>%
      dplyr::select(available_geo) %>%
      unlist()

    supp_geo_level <- supp_geo_level_str %>%
      stringr::str_split(",", simplify = TRUE) %>%
      stringr::str_trim()

    if (!(param$geo_level %in% supp_geo_level)) {
      geo_level_error <- paste("Option geo_level must be one of", supp_geo_level_str)

      stop(geo_level_error)
    }

  }

}
