# actions/scripts/check_comex_updates.R
#
# Checa se o dado da COMEX atualizou comparando o último mês/ano disponível
# na fonte oficial (página de dados abertos do MDIC e API do ComexStat) com o
# último mês/ano retornado pelo pacote datazoom.amazonia (load_br_trade).
#
# Contrato:
# - Se o último mês/ano for IGUAL: encerra o script com mensagem limpa e exit code 0.
# - Se for DIFERENTE: rebaixa todos os dados da COMEX (todos os anos de available_time
#   para os 4 datasets: export_mun, import_mun, export_prod, import_prod).
#
# Flags CLI opcionais:
#   --force        Força o re-download mesmo se mês/ano forem iguais (útil para testes)
#   --anos <anos>  Restringe anos para re-download (ex: --anos 2026 ou --anos 2025:2026)

suppressPackageStartupMessages({
  library(datazoom.amazonia)
  library(httr)
  library(jsonlite)
  library(stringr)
  library(dplyr)
})

args_cli <- commandArgs(trailingOnly = TRUE)
flag_force <- "--force" %in% args_cli
flag_anos_idx <- which(args_cli == "--anos")
custom_anos <- NULL
if (length(flag_anos_idx) > 0 && length(args_cli) >= flag_anos_idx + 1) {
  custom_anos <- eval(parse(text = args_cli[flag_anos_idx + 1]))
}

options(timeout = max(1200, getOption("timeout")))

# 1. Consulta último mês/ano na fonte oficial (MDIC / ComexStat)
obter_ultimo_mes_ano_fonte <- function() {
  meses_pt <- c(
    "janeiro" = 1L, "fevereiro" = 2L, "março" = 3L, "marco" = 3L,
    "abril" = 4L, "maio" = 5L, "junho" = 6L, "julho" = 7L,
    "agosto" = 8L, "setembro" = 9L, "outubro" = 10L, "novembro" = 11L, "dezembro" = 12L
  )

  url_pagina <- "https://www.gov.br/mdic/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta"
  txt <- tryCatch({
    resp <- httr::GET(url_pagina, httr::timeout(30))
    httr::stop_for_status(resp)
    httr::content(resp, as = "text", encoding = "UTF-8")
  }, error = function(e) NULL)

  if (!is.null(txt)) {
    m <- stringr::str_match(
      txt,
      stringr::regex(
        "[uú]ltimos\\s+dados\\s+dispon[ií]veis\\s*:\\s*(?:[a-zç]+\\s*[-a–]\\s*)?([a-zç]+)\\s+de\\s+((?:19|20)\\d{2})",
        ignore_case = TRUE
      )
    )
    if (!is.na(m[1, 1])) {
      mes_val <- unname(meses_pt[tolower(m[1, 2])])
      ano_val <- as.integer(m[1, 3])
      if (!is.na(mes_val) && !is.na(ano_val)) {
        return(list(ano = ano_val, mes = mes_val, fonte = "gov.br/mdic"))
      }
    }
  }

  # Fallback: API ComexStat
  url_api <- "https://api-comexstat.mdic.gov.br/general/dates/updated"
  resp_api <- httr::GET(url_api, httr::timeout(30))
  httr::stop_for_status(resp_api)
  obj <- jsonlite::fromJSON(httr::content(resp_api, as = "text", encoding = "UTF-8"))
  list(
    ano   = as.integer(obj$data$year),
    mes   = as.integer(obj$data$monthNumber),
    fonte = "api-comexstat.mdic.gov.br"
  )
}

# 2. Descobre os anos cobertos no manifesto do datazoom.amazonia
time_str <- tryCatch(
  datazoom.amazonia:::dataset_meta("comex", "export_mun", "available_time"),
  error = function(e) NULL
)
if (is.null(time_str) || is.na(time_str)) {
  stop("Não foi possível ler available_time para comex/export_mun no manifesto do pacote.")
}

anos_pacote <- datazoom.amazonia:::parse_years(time_str)
ano_mais_recente <- max(anos_pacote)

message("Ano mais recente no manifesto do datazoom.amazonia: ", ano_mais_recente)

# 3. Baixa o ano mais recente via load_br_trade para checar o último mês retornado pelo pacote
message("Consultando último mês/ano retornado por datazoom.amazonia::load_br_trade()...")
amostra_recente <- load_br_trade(
  dataset = "export_mun",
  language = "pt",
  time_period = ano_mais_recente,
  raw_data = FALSE
)

if (!all(c("ano", "mes") %in% names(amostra_recente))) {
  stop("O retorno de load_br_trade() não contém as colunas 'ano' e 'mes'.")
}

max_ano_pacote <- as.integer(max(amostra_recente$ano, na.rm = TRUE))
max_mes_pacote <- as.integer(max(amostra_recente$mes[amostra_recente$ano == max_ano_pacote], na.rm = TRUE))
info_pacote <- list(ano = max_ano_pacote, mes = max_mes_pacote)

info_fonte <- obter_ultimo_mes_ano_fonte()

fmt_ym <- function(x) sprintf("%02d/%d", x$mes, x$ano)

message(
  "Check COMEX:\n",
  "  - Fonte oficial (", info_fonte$fonte, "): ", fmt_ym(info_fonte), "\n",
  "  - Pacote datazoom.amazonia: ", fmt_ym(info_pacote)
)

# 4. Decisão: se igual, encerra o script; se diferente, rebaixa todos os dados
dados_iguais <- identical(info_fonte$ano, info_pacote$ano) &&
  identical(info_fonte$mes, info_pacote$mes)

if (dados_iguais && !flag_force) {
  message("O último mês/ano é IGUAL (", fmt_ym(info_fonte), "). Nenhum dado novo na COMEX. Encerrando o script.")
  quit(save = "no", status = 0)
}

if (flag_force) {
  message("Flag --force detectada: forçando re-download mesmo com meses/anos coincidentes.")
} else {
  message("O último mês/ano é DIFERENTE (Fonte: ", fmt_ym(info_fonte), " vs Pacote: ", fmt_ym(info_pacote), ").")
}

anos_para_baixar <- if (!is.null(custom_anos)) custom_anos else anos_pacote
message("Iniciando re-download dos dados da COMEX (anos ", min(anos_para_baixar), " a ", max(anos_para_baixar), ")...")

datasets_comex <- c("export_mun", "import_mun", "export_prod", "import_prod")
total_linhas <- 0

for (ds in datasets_comex) {
  message("\n--- Rebaixando dataset: ", ds, " (", min(anos_para_baixar), " a ", max(anos_para_baixar), ") ---")
  res <- load_br_trade(
    dataset = ds,
    language = "pt",
    time_period = anos_para_baixar,
    raw_data = FALSE
  )
  total_linhas <- total_linhas + nrow(res)
  message("Sucesso para ", ds, ": ", nrow(res), " linhas baixadas e tratadas.")
}

message("\nTodos os dados da COMEX foram rebaixados com sucesso! Total de linhas: ", total_linhas)
