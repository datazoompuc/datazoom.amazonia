#' @title Greenhouse gases emission estimates (SEEG)
#'
#' @description Loads data of estimates of emission of greenhouse gases
#'
#' @param dataset A dataset name ("seeg_agro", "seeg_industry", "seeg_energy", "seeg_land" and "seeg_residuals").
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality".
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#'
#' @return A \code{tibble} with the selected data.
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples \dontrun{
#' # download state raw data
#' seeg <- load_seeg(dataset = 'seeg',
#'                   raw_data = TRUE,
#'                   geo_level = "state")
#' }

load_seeg <- function(dataset = "seeg", raw_data,
                      geo_level, language = "eng"){


  survey <- link <- id_code <- x1970 <- x2019 <- nivel_1_setor <- nivel_2 <- nivel_3 <- nivel_4 <- nivel_5 <- nivel_6 <- produto <- atvidade_economica <- Valor <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$geo_level = geo_level
  param$language = language
  param$raw_data = raw_data

  if (!is.numeric(param$dataset)){
    param$code = datasets_seeg() %>%
      dplyr::filter(dataset == param$dataset) %>%
      dplyr::select(id_code) %>%
      unlist() %>%
      as.numeric()
  } else {param$code = param$dataset}


  ## Dataset

  if (is.null(param$dataset)){stop('Missing Dataset!')}
  if (is.null(param$raw_data)){stop('Missing TRUE/FALSE for Raw Data')}


  ##############
  ## Download ##
  ##############

  if (param$geo_level == "municipality"){message("Please follow the steps from `googledrive` package to download the data. This may take a while.")}

  dat <- external_download(dataset = param$dataset,
                           source = 'seeg',
                           geo_level = param$geo_level)

  dat = dat %>%
    janitor::clean_names() %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.character,function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")})


  ## Return Raw Data
  if (raw_data == TRUE){return(dat)}

if (param$dataset == "seeg_agro"){

  ## Create Longer Data - Years as a Variable

  dat = dat %>%
    tidyr::pivot_longer(
      cols = x1970:x2019,
      names_to = 'Ano',
      names_prefix = 'x',
      values_to = 'Valor'
    )
  ## Changing column name, filtering by the specific sector and harmonizing variables
  dat = dat %>%
    dplyr::filter(nivel_1_setor == "Agropecuaria") %>%
    dplyr::rename(setor = nivel_1_setor, processos_geradores_emissoes = nivel_2,
           tipo_emissao = nivel_3,
           emissores_diretos_e_indiretos = nivel_4,
           fonte_de_emissoes = nivel_5,
           categorias_dos_emissores = nivel_6)%>%
    dplyr::mutate(atividade_economica = dplyr::case_when(atividade_economica == "PEC" ~ "Pecuaria",
                                           atividade_economica == "AGR" ~ "Agricultura"))%>%
    dplyr::mutate(produto = dplyr::case_when(produto == "ALIM_BEBIDAS" ~ "Alimentos/Bebidas",
                               produto == "CAR" ~ "Carro",
                               produto == "CAR/LEI" ~ "Carro/Leite",
                               produto == "CAR/LEI/ALIM_BEBIDAS" ~ "Carro/Leite/Alimentos/Bebidas",
                               produto == "LEI" ~ "Leite"))



}

if (param$dataset == "seeg_industry"){

  ## Create Longer Data - Years as a Variable

  dat = dat %>%
    tidyr::pivot_longer(
      cols = x1970:x2019,
      names_to = 'Ano',
      names_prefix = 'x',
      values_to = 'Valor'
    )
  ## Changing column name, filtering by the specific sector and harmonizing variables
  dat = dat %>%
    dplyr::filter(nivel_1_setor == "Processos Industriais") %>%
    dplyr::filter(!is.na(produto))%>%
    dplyr::rename(setor = nivel_1_setor, processos_geradores_emissoes = nivel_2,
           produtos_emissores = nivel_3,
           destinacao_produto = nivel_4,
           fonte_energetica = nivel_5)%>%
  dplyr::select(-nivel_6) %>%
    dplyr::mutate(atividade_economica = dplyr::case_when(atividade_economica == "CIM" ~ "Industria Cimenteira",
                                           atividade_economica == "ENE_ELET" ~ "Industria Energia Eletrica",
                                           atividade_economica == "MET" ~ "Industria Metaleira",
                                           atividade_economica == "Outra_IND" ~ "Outra Industria"))%>%
    dplyr::mutate(produto = case_when(produto == "ALU" ~ "Aluminio"))

}

if (param$dataset == "seeg_energy"){

  ## Create Longer Data - Years as a Variable

  dat = dat %>%
    tidyr::pivot_longer(
      cols = x1970:x2019,
      names_to = 'Ano',
      names_prefix = 'x',
      values_to = 'Valor'
    )
  ## Changing column name, filtering by the specific sector and harmonizing variables
  dat = dat %>%
    dplyr::filter(nivel_1_setor == "Energia")%>%
    dplyr::filter(!is.na(produto)) %>%
    dplyr::rename(setor = nivel_1_setor, tipo_emissao = nivel_2,
           processos_geradores_emissoes = nivel_3,
           atividade_geradora = nivel_4,
           fonte_energetica = nivel_5,
           destinacao_atividade = nivel_6)%>%
    dplyr::mutate(produto = dplyr::case_when(produto == "ALIM_BEBIDAS" ~ "Alimentos/Bebidas",
                               produto == "ENE_ELET" ~ "Energia Eletrica",
                               produto == "ALU" ~ "Aluminio")) %>%
      dplyr::select(-atividade_economica)

}

if (param$dataset == "seeg_land"){
  ## Create Longer Data - Years as a Variable

  dat = dat %>%
    tidyr::pivot_longer(
      cols = x1970:x2019,
      names_to = 'Ano',
      names_prefix = 'x',
      values_to = 'Valor'
    )
  ## Changing column name, filtering by the specific sector and harmonizing variables
  dat = dat %>%
    dplyr::filter(nivel_1_setor == "Mudanca de Uso da Terra e Floresta") %>%
    dplyr::filter(!is.na(Valor)) %>%
    dplyr::rename(setor = nivel_1_setor, processos_geradores_emissoes = nivel_2,
           bioma = nivel_3,
           area_bioma = nivel_4,
           tipo_atividade_geradora = nivel_5,
           local_atividade_geradora = nivel_6) %>%
    dplyr::mutate(atividade_economica = dplyr::case_when(atividade_economica == "AGROPEC" ~ "Agropecuaria")) %>%
    dplyr::select(-produto)


}

if (param$dataset == "seeg_residuals"){

  ## Create Longer Data - Years as a Variable

  dat = dat %>%
    tidyr::pivot_longer(
      cols = x1970:x2019,
      names_to = 'Ano',
      names_prefix = 'x',
      values_to = 'Valor'
    )
  ## Changing column name, filtering by the specific sector and harmonizing variables
  dat = dat %>%
    dplyr::filter(nivel_1_setor == "Residuos") %>%
    dplyr::filter(!is.na(produto)) %>%
    dplyr::rename(setor = nivel_1_setor, categorias_emissao = nivel_2,
           processos_geradores_emissoes = nivel_3,
           atividade_geradora = nivel_4,
           categorias_processos_geradores = nivel_5) %>%
    dplyr::select(-nivel_6) %>%
    dplyr::mutate(atividade_economica = dplyr::case_when(atividade_economica == "PEC" ~ "Pecuaria",
                                           atividade_economica == "Outra_IND" ~ "Outra Industria"
                                           )) %>%
    dplyr::mutate(produto = dplyr::case_when(produto == "ALIM_BEBIDAS" ~ "Alimentos/Bebidas",
                               produto == "LEI" ~ "Leite",
                               produto == "CAR" ~ "Carro",
                               produto == "CAR/LEI" ~ "Carro/Leite",
                               produto == "ALU" ~ "Aluminio",
                               produto == "ENE_ELET" ~ "Energia Eletrica",
                               produto == "CAR/LEI/ALIM_BEBIDAS" ~ "Carro/Leite/Alimentos/Bebidas"))
}

  return(dat)
}


