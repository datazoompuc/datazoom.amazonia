# actions/scripts/check_all_loads.R

args_cli <- commandArgs(trailingOnly = TRUE)
target_fn <- NULL

 if (length(args_cli) >= 2 && args_cli[1] == "--fn") target_fn <- args_cli[2]



library(datazoom.amazonia)

# Config
TIME_PERIOD <- 2020
LANGUAGE    <- "pt"
RAW_DATA    <- FALSE
GEO_LEVEL   <- "state"



datasets_by_fn <- list(
  load_aneel = c(
    "energy_development_budget",
    "energy_generation",
    "energy_enterprises_distributed"
  ),
  load_baci  = "HS92",
  load_br_trade = c("export_mun",
                    "import_mun",
                    "export_prod",
                    "import_prod"
  ),
  load_cempre   = "cempre",
  load_censo_agro = c(
    "agricultural_land_area",
    "agricultural_area_use",
    "agricultural_employees_tractors",
    "agricultural_producer_condition",
    "animal_species",
    "animal_products",
    "vegetable_production_area",
    "vegetable_production_permanent",
    "vegetable_production_temporary",
    "livestock_production"
  ),
  load_climate = c(
    "max_temperature",
    "min_temperature",
    "wind_speed",
    "vapor_pressure_deficit",
    "vapor_pressure",
    "snow_water_equivalent",
    "shortwave_radiation_flux",
    "soil_moisture",
    "runoff",
    "precipitation",
    "potential_evaporation",
    "climatic_water_deficit",
    "water_evaporation",
    "palmer_drought_severity_index"
  ),
  load_degrad = "degrad",
  load_deter = c("deter_amz",
                 "deter_cerrado"
  ),
  load_epe = c(
    "consumer_energy_consumption",
    "industrial_energy_consumption",
    "national_energy_balance"
  ),
  load_ibama = c("embargoed_areas",
                 "distributed_fines",
                 "collected_fines"
  ),
  load_iema = "iema",
  load_imazon = "imazon_shp",
  load_ips = "all",
  load_mapbiomas = c(
    "mapbiomas_cover",
    "mapbiomas_transition",
    "mapbiomas_irrigation",
    "mapbiomas_deforestation_regeneration",
    "mapbiomas_mining",
    "mapbiomas_water",
    "mapbiomas_fire"
  ),
  load_pam = "all_crops",
  load_pevs = c(
    "pevs_forest_crops",
    "pevs_silviculture",
    "pevs_silviculture_area"
  ),
  load_pibmunic = "pibmunic",
  load_population = "population",
  load_ppm = c(
    "ppm_livestock_inventory",
    "ppm_sheep_farming",
    "ppm_animal_origin_production",
    "ppm_cow_farming",
    "ppm_aquaculture"
  ),
  load_prodes = c(
    "deforestation",
    "residual_deforestation",
    "native_vegetation",
    "hydrography",
    "non_forest",
    "clouds"
  ),
  load_seeg = c(
    "seeg",
    "seeg_farming",
    "seeg_industry",
    "seeg_energy",
    "seeg_land",
    "seeg_residuals"
  ),
  load_sigmine = "sigmine_active"
)

# -- Helper -- #
call_with_geo <- function(fn, args, geo_level = "state") {

  if (!"geo_level" %in% names(formals(fn))) {
    return(do.call(fn, args))
  }

  # tenta com geo_level = "state"
  res <- tryCatch(
    do.call(fn, c(args, list(geo_level = geo_level))),
    error = function(e) e
  )

  # se funcionou, retorna
  if (!inherits(res, "error")) {
    return(res)
  }

  # fallback: tenta sem geo_level
  do.call(fn, args)
}


# lista todas as funções exportadas do pacote que começam com "load_"
# Exceto load_datasus
fns <- ls("package:datazoom.amazonia")
get_fns <- fns[startsWith(fns, "load_") & !endsWith(fns, "datasus")]

if (!is.null(target_fn)) {
  if (!target_fn %in% get_fns){
    stop("Função especificada em --fn não encontrada: ", target_fn)
  }
  get_fns <- intersect(get_fns, target_fn)
}

results <- data.frame(
  function_name = character(),
  dataset       = character(),
  status        = character(),
  message       = character(),
  stringsAsFactors = FALSE
)


for (fn_name in get_fns) {
  fn <- get(fn_name, envir = asNamespace("datazoom.amazonia"))
  arg_names <- names(formals(fn))

  cat("\nTestando", fn_name, "...\n")

  ds_vec <- datasets_by_fn[[fn_name]]
  if (is.null(ds_vec)) {
    results <- rbind(results, data.frame(
      function_name = fn_name,
      dataset       = NA_character_,
      status        = "SKIP",
      message       = "Sem datasets_by_fn definido para esta função",
      stringsAsFactors = FALSE
    ))
    next
  }

  for (ds in ds_vec) {
    cat("  - dataset =", ds, "\n")

    args <- list()
    if ("dataset"     %in% arg_names) args$dataset     <- ds
    if ("time_period" %in% arg_names) args$time_period <- TIME_PERIOD
    if ("raw_data"    %in% arg_names) args$raw_data    <- RAW_DATA
    if ("language"    %in% arg_names) args$language    <- LANGUAGE

    res <- tryCatch(
      {
        out <- call_with_geo(fn, args, geo_level = GEO_LEVEL)

        if (!is.null(out) && (is.data.frame(out) || inherits(out, "tbl_df"))) {
          "OK"
        } else {
          "WEIRD"
        }
      },
      warning = function(w) paste("WARN:", conditionMessage(w)),
      error   = function(e) paste("ERROR:", conditionMessage(e))
    )

    status <- if (identical(res, "OK")) {
      "OK"
    } else if (identical(res, "WEIRD")) {
      "WEIRD"
    } else if (startsWith(res, "WARN:")) {
      "WARN"
    } else {
      "FAIL"
    }

    msg <- if (status %in% c("WARN", "FAIL", "WEIRD")) res else ""

    results <- rbind(results, data.frame(
      function_name = fn_name,
      dataset       = ds,
      status        = status,
      message       = if (status %in% c("WARN","FAIL","WEIRD")) res else "",
      stringsAsFactors = FALSE
    ))
  }
}

print(results)

# salva resultados
out_name <- if (!is.na(target_fn)) {
  sprintf("check_results_%s_%s.csv", target_fn, Sys.Date())
} else {
  sprintf("check_results_all_%s_.csv", Sys.Date())
}

write.csv(results, out_name, row.names = FALSE)

cat("Resultados salvos em:", out_name, "\n")

# Falha só se tiver FAIL (não WARN/WEIRD)
if (any(results$status == "FAIL")) {
  stop("Uma ou mais funções load_ falharam. Veja o log acima.")
}
