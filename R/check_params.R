check_params <- function(param, source){

  ## checking if dataset is supported

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

  # checking if time_period is supported

  if (!is.null(param$time_period)){

    # constructing vector of supported years

    supp_time_period_str <- datasets_link() %>%
      dplyr::filter(survey == source, dataset == param$dataset) %>%
      select(available_time)

    # separating by commas: 2003, 2007, 2014 -> c(2003, 2007, 2014)

    supp_time_period <- supp_time_period_str %>%
      stringr::str_split(",", simplify = TRUE)

    # separating by hyphens: 2003 - 2007 -> 2003:2007

    supp_time_period <- supp_time_period %>%
      stringr::str_replace("-", ":") %>%
      parse(text = .) %>%
      eval()

    if (!(param$time_period %in% supp_time_period)) {
      time_period_error <- paste("Time period", param$time_period, "not supported.",
                             "Time period must be in", supp_time_period_str)

      stop(time_period_error)
    }

  }

}
