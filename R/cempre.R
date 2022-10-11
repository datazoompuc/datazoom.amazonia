#' @title CEMPRE - Central Register of Companies
#'
#' @description Loads information on companies and other organizations and their respective formally constituted local units, registered with the CNPJ - National Register of Legal Entities.
#'
#' @param dataset A dataset name ("cempre").
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality".
#' @param sectors A \code{boolean} that defines if the data will be return separated by sectors (\code{TRUE}) or not (\code{FALSE}).
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # Download raw data (raw_data = TRUE) at the country level
#' # from 2008 to 2010 (time_period = 2008:2010).
#' data <- load_cempre(
#'   raw_data = TRUE,
#'   geo_level = "country",
#'   time_period = 2008:2010
#' )
#'
#' # Download treted data (raw_data = FALSE) by state (geo_level = "state")
#' # from 2008 to 2010 (time_period = 2008:2010) in portuguese (language = "pt").
#' # In this example, data is split by sector (sectors = TRUE)
#' data <- load_cempre(
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   time_period = 2008:2010,
#'   language = "pt",
#'   sectors = TRUE
#' )
#' }
#'
#' @export

load_cempre <- function(dataset = "cempre", raw_data = FALSE,
                        geo_level, time_period,
                        language = "eng", sectors = FALSE) {

  ###########################
  ## Bind Global Variables ##
  ###########################

  sidra_code <- available_time <- legal_amazon <- municipio_codigo <- ano <- NULL
  ano_codigo <- classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo <- NULL
  geo_id <- id_code <- nivel_territorial <- nivel_territorial_codigo <- valor <- NULL
  variavel <- unidade_de_medida <- unidade_de_medida_codigo <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$geo_level <- geo_level
  param$time_period <- time_period
  param$language <- language

  if (!is.numeric(param$dataset)) {
    param$code <- datasets_link() %>%
      dplyr::filter(dataset == param$dataset) %>%
      dplyr::select(sidra_code) %>%
      unlist() %>%
      as.numeric()
  } else {
    param$code <- param$dataset
  }

  ## Check if year is acceptable

  year_check <- datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(available_time) %>%
    unlist() %>%
    as.character() %>%
    stringr::str_split(pattern = "-") %>%
    unlist() %>%
    as.numeric()

  if (min(time_period) < year_check[1]) {
    stop("Provided time period less than supported. Check documentation for time availability.")
  }
  if (max(time_period) > year_check[2]) {
    stop("Provided time period greater than supported. Check documentation for time availability.")
  }

  ##############
  ## Download ##
  ##############

  # We need to show year that is being downloaded as well
  # Heavy Datasets may take several minutes

  if (sectors & geo_level == "municipality") {
    warning("This may take too long")
  }

  ## Download separate by sectors
  if (sectors) {
    cnaes <- list(
      "117897", "116830", "116880", "116910", "117296",
      "117307", "117329", "117363", "117484", "117543",
      "117555", "117608", "117666", "117673", "117714",
      "117774", "117788", "117810", "117838", "117861",
      "117888", "117892"
    )

    year_cnaes <- purrr::cross2(as.character(param$time_period), cnaes)

    dat <- year_cnaes %>%
      purrr::map(function(year_cnae) {
        sidra_download(
          sidra_code = param$code,
          year = year_cnae[[1]],
          geo_level = param$geo_level,
          classific = c("C12762"),
          category = list(year_cnae[[2]])
        )
      }) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()

    ## Download only the total
  } else {
    cnaes <- list("117897")

    dat <- as.list(as.character(param$time_period)) %>%
      purrr::map(function(year_num) {
        sidra_download(
          sidra_code = param$code,
          year = year_num,
          geo_level = param$geo_level,
          classific = c("C12762"),
          category = cnaes
        )
      }) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()
  }

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################


  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })


  dat <- dat %>%
    dplyr::select(-c(nivel_territorial_codigo, nivel_territorial, ano_codigo)) %>%
    dplyr::mutate(valor = as.numeric(valor))

  ## Only Keep Valid Observations

  dat <- dat %>%
    dplyr::filter(!is.na(valor))


  if (geo_level == "country") {
    dat$geo_id <- dat$brasil
    dat <- dplyr::select(dat, -"brasil_codigo", -"brasil")
  }

  if (geo_level == "state") {
    dat$geo_id <- dat$unidade_da_federacao_codigo
    dat <- dplyr::select(dat, -"unidade_da_federacao_codigo", -"unidade_da_federacao")
  }

  if (geo_level == "municipality") {
    dat$geo_id <- dat$municipio_codigo
    dat <- dplyr::select(dat, -"municipio", -"municipio_codigo")
  }


  ################################
  ## Harmonizing Variable Names ##
  ################################

  dat <- dat %>%
    dplyr::select(-unidade_de_medida, -unidade_de_medida_codigo)

  if (sectors == TRUE) {
    dat <- dat %>%
      dplyr::mutate(id_code = dplyr::case_when(
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117897 ~ "117897_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117897 ~ "117897_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117897 ~ "117897_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117897 ~ "117897_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116830 ~ "116830_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116830 ~ "116830_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116830 ~ "116830_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116830 ~ "116830_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116880 ~ "116880_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116880 ~ "116880_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116880 ~ "116880_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116880 ~ "116880_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116910 ~ "116910_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116910 ~ "116910_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116910 ~ "116910_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116910 ~ "116910_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117296 ~ "117296_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117296 ~ "117296_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117296 ~ "117296_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117296 ~ "117296_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117307 ~ "117307_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117307 ~ "117307_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117307 ~ "117307_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117307 ~ "117307_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117329 ~ "117329_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117329 ~ "117329_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117329 ~ "117329_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117329 ~ "117329_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117363 ~ "117363_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117363 ~ "117363_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117363 ~ "117363_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117363 ~ "117363_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117484 ~ "117484_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117484 ~ "117484_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117484 ~ "117484_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117484 ~ "117484_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117543 ~ "117543_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117543 ~ "117543_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117543 ~ "117543_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117543 ~ "117543_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117555 ~ "117555_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117555 ~ "117555_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117555 ~ "117555_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117555 ~ "117555_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117608 ~ "117608_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117608 ~ "117608_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117608 ~ "117608_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117608 ~ "117608_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117666 ~ "117666_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117666 ~ "117666_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117666 ~ "117666_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117666 ~ "117666_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117673 ~ "117673_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117673 ~ "117673_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117673 ~ "117673_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117673 ~ "117673_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117714 ~ "117714_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117714 ~ "117714_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117714 ~ "117714_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117714 ~ "117714_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117774 ~ "117774_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117774 ~ "117774_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117774 ~ "117774_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117774 ~ "117774_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117788 ~ "117788_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117788 ~ "117788_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117788 ~ "117788_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117788 ~ "117788_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117810 ~ "117810_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117810 ~ "117810_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117810 ~ "117810_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117810 ~ "117810_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117838 ~ "117838_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117838 ~ "117838_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117838 ~ "117838_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117838 ~ "117838_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117861 ~ "117861_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117861 ~ "117861_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117861 ~ "117861_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117861 ~ "117861_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117888 ~ "117888_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117888 ~ "117888_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117888 ~ "117888_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117888 ~ "117888_4",
        variavel_codigo == 2585 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117892 ~ "117892_1",
        variavel == "Pessoal ocupado total" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117892 ~ "117892_2",
        variavel == "Pessoal ocupado assalariado" &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117892 ~ "117892_3",
        variavel_codigo == 662 &
          classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117892 ~ "117892_4"
      ))
  }



  dat <- dat %>%
    dplyr::select(-"classificacao_nacional_de_atividades_economicas_cnae_2_0") %>%
    dplyr::arrange(classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo, variavel) %>%
    tidyr::pivot_wider(
      id_cols = c(ano, geo_id),
      names_from = c(variavel, id_code),
      values_from = valor,
      names_sep = "_V",
      values_fn = sum,
      values_fill = NA
    ) %>%
    janitor::clean_names()

  labelled <- function(x, label) {
    Hmisc::label(x) <- label
    x
  }

  label_data_eng <- function(df, cols, dic) {
    label_value <- as.character(dic[dic$id_code == cols, "var_eng"])

    df <- df %>%
      dplyr::mutate_at(
        dplyr::vars(tidyr::matches(cols)),
        ~ labelled(., as.character(dic[dic$id_code == cols, "var_eng"]))
      )

    return(df)
  }


  label_data_pt <- function(df, cols, dic) {
    label_value <- as.character(dic[dic$id_code == cols, "var_pt"])

    df <- df %>%
      dplyr::mutate_at(
        dplyr::vars(tidyr::matches(cols)),
        ~ labelled(., as.character(dic[dic$id_code == cols, "var_pt"]))
      )

    return(df)
  }



  ## Load Dictionary

  dic <- load_dictionary(param$dataset)

  types <- as.character(dic$id_code)
  types <- types[types != "0"] ## Remove 0


  if (language == "eng") {
    for (i in seq_along(types)) {
      dat <- label_data_eng(dat, cols = types[i], dic = dic)
    }
  }

  if (language == "pt") {
    for (i in seq_along(types)) {
      dat <- label_data_pt(dat, cols = types[i], dic = dic)
    }
  }

  remove_numbers <- function(string) {
    stringr::str_remove(string = string, pattern = "_\\d")
  }

  ##########################
  ## Returning Data Frame ##
  ##########################

  return(dat)
}
