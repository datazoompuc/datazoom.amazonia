# datazoom.amazonia (development version)

  * Fixed: `energy_enterprises_distributed`'s dictionary (`R/dictionary.R`) had a completely stale `dsc_modalidade_habilitado` group -- all 4 of its old `var_code`s (`"Com Microgeracao ou Minigeracao distribuida"`, `"Caracterizada como Autoconsumo remoto"`, ...) were long descriptive strings matching zero real values; the source now uses the same short codes (`Geracao na propria UC`/`Auto consumo remoto`/`Compartilhada`/`Condominio`) `R/aneel.R`'s separate `case_when()` block already expects for its own, different `business_type` column, so `business_type_description` (the dictionary-driven one) was passing every value through completely untranslated in both `pt` and `eng`. `dsc_classe_consumo` and `dsc_fonte_geracao` were correct as far as they went but incomplete (missing `Comercial`/`Consumo Proprio`/`Industrial`/`Rural` and `Etanol`/`Oleo Diesel`/`Oleos vegetais` respectively). Verified live against the real ~4.66M-row `empreendimento-geracao-distribuida.csv`: every real value in all 4 groups now translates correctly except `dsc_classe_consumo`'s `"REBR"`/`"REBR "` (two variants, one with a trailing space, in ANEEL's own data) -- left untranslated, no documented meaning found. See `tests/testthat/test-aneel-schema.R`.

  * Fixed: `energy_development_budget`'s dictionary (`R/dictionary.R`) had 17 `"tipo_de_despesa"` rows (CAFT CCEE, Programa Luz para Todos, Subsidio Baixa Renda, ...) that never matched any real column -- the manifest's current resource (`cde-beneficiarios-rede-basica-*.csv`, a per-agent "Rede Basica" tariff-subsidy beneficiary listing) has no expense-by-category dimension at all; `"tipo_de_despesa"` doesn't exist as a column in it. Checked against a locally saved copy of the real 2023 file (`dadosabertos.aneel.gov.br` was unreachable at investigation time -- TLS handshake failing across `curl`, `httr`, and `utils::download.file()` alike, most likely a rate-limit from this session's own heavy traffic). Dropped the 17 dead rows rather than keep them as dead weight describing a dimension the current data doesn't have (same "don't collapse or preserve a category that isn't really there" principle as the manifest-maintenance skill's schema-migration rule) -- if ANEEL's catalogue turns out to still carry a separate expense-by-category CDE resource, that's a new dataset/variable to add, not something to fold back into this one. Replaced with the one column that has an unambiguous, verified value: `dsc_grupo_tarifario` is always `"Rede Basica"` in the real data, now translated to `"rede_basica"`/`"basic_network"`. `idc_classe_consumidor` (real values: 9, 10) and `idc_tipo` (real values: 7, 8) are bare ANEEL indicator codes with no documented meaning found -- deliberately left untranslated at the value level (their column names are still translated, via the `eng` rename map fixed below). Matching `"tipo_de_despesa" = "type_of_expense"` rename-map entry also removed. See `tests/testthat/test-aneel-schema.R`.

  * Fixed: `load_aneel(..., language = "eng")`'s column-renaming map (`R/aneel.R`) was stale for `energy_generation` and `energy_development_budget` -- about half of `energy_generation`'s real columns and all but two of `energy_development_budget`'s stayed in raw Portuguese `snake_case` in `eng` output instead of getting English names, because the map's keys were written for a column-naming convention that doesn't match either dataset's real `janitor::clean_names()` output today (same root cause, and largely the same wrong keys, as the `energy_generation` dictionary mismatch fixed above -- `"fase"`/`"origem"`/`"tipo"`/`"geracao_qualificada"`/`"inicio_vigencia"`/`"fim_vigencia"`/`"proprietario_regime_de_exploracao"`/`"sub_bacia"`/`"municipio_s"`/`"entrada_em_operacao"` never matched real columns; real ones are `dsc_fase_usina`/`dsc_origem_combustivel`/`dsc_fonte_combustivel`/`idc_geracao_qualificada`/`dat_inicio_vigencia`/`dat_fim_vigencia`/`dsc_propri_regime_pariticipacao`/`dsc_sub_bacia`/`dsc_muninicpios`/`dat_entrada_operacao`, the last two carrying ANEEL's own typos). Fixed the mismatched keys and added the map entries these two datasets were missing entirely (`nom_empreendimento`, `mda_potencia_outorgada_kw`, `num_ano`, `vlr_desconto`, etc.) -- verified live: every real column in both datasets now gets an English name in `eng` mode, no duplicates. Also removed a dead `as.Date(operation_start, format = "%d/%m/%Y")` step that had matched zero columns in any of the three datasets since it was written (nothing is ever literally named `operation_start` pre-rename) -- moot anyway, since `energy_generation`'s real date column (`dat_entrada_operacao`) turns out to already be `IDate`/`Date` straight out of `fread()`. `fragility-notes` test's `min_tags` for `R/aneel.R` dropped from 5 to 4 accordingly. `energy_development_budget`'s dictionary (`"tipo_de_despesa"`) was left as a documented, unfixed gap when this entry was first written -- see the entry below for how it was resolved.

  * Fixed: `aneel` was excluded from the session-scoped file download cache (`R/download.R`'s `use_cache <- download_cache_enabled() && source != "aneel"`, added in a 2026-09-18 merge commit as "per maintainer decision"), so every `load_aneel()` call re-downloaded its file from scratch even within the same R session, back-to-back. That exclusion's real justification (a shared, mutable-by-reference `data.table::fread()` result being unsafe to reuse under `raw_data = TRUE`) belongs to the separate, already-correctly-scoped *parsed-object* cache (`parsed_cache_eligible()`, `.xlsx` reads only -- aneel was never in it) -- the *file-level* cache just remembers a download's disk location and shares no object, so there was no actual risk to guard against. Removed the `source != "aneel"` exclusion; the parsed-object cache exclusion is untouched. Verified live: a second `load_aneel()` call for the same dataset in one session no longer re-downloads (confirmed against `energy_generation`: 7.2s → 0.25s, no second network request). See `tests/testthat/test-download-cache.R`.

  * Fixed: `load_aneel(dataset = "energy_enterprises_distributed")` hard-errored on every real download (`data.table::fread()` handed the raw zip bytes directly -- `"string with embedded nul: 'PK\003\004...'"`, the zip magic number). `R/download.R`'s `aneel` branch hardcoded `file_extension <- ".csv"` for all three aneel datasets, but this one's manifest URL is a real ~106MB zip (`empreendimento-geracao-distribuida.zip`, ~1.5GB uncompressed) -- the unzip step (keyed on `file_extension == ".zip"`) never ran. Predates the `energy_generation` migration above; traced to this dataset's original commit (April 2023). Now sets `file_extension <- ".zip"` for this dataset specifically and reads the extracted CSV by pattern inside the existing `.zip`-handling dispatch, alongside `sigmine`/`ibama`'s shapefile-by-pattern precedent. Verified live: downloads and parses all ~4.66M rows.

  * Fixed: all three `load_aneel()` datasets were being read with the wrong `fread()` `encoding` argument. `energy_enterprises_distributed` and `energy_development_budget` used `encoding = "Latin-1"` (assumed since `energy_enterprises_distributed`'s original commit, April 2023) with a follow-up `iconv(from = "ISO-8859-1", to = "UTF-8")` step in `R/aneel.R` meant to undo it -- but live inspection of both sources' raw bytes (2026-09-21) showed they're genuinely UTF-8 (e.g. `"Condomínio"`/`"Rede Básica"` are real multi-byte UTF-8 sequences, not single-byte Latin-1 characters), so `fread(encoding = "Latin-1")` was misreading valid UTF-8 as Latin-1, producing mojibake (real `"Condomínio"` came through as `"CondomÃ­nio"`) that no post-hoc `iconv()` could cleanly reverse. Both datasets now read `encoding = "UTF-8"` directly; the now-unnecessary `iconv()` step was removed from `R/aneel.R`. Also fixed alongside: `vlr_desconto`/`vlr_cobranca` (CDE discount/charge amounts, `energy_development_budget`) were never parsed to numeric at all -- same root cause and same fix as `energy_generation`'s `mda_*`/`num_coord_*` gap below, generalized to a shared prefix list (`mda`/`num_coord`/`vlr`) covering all three datasets that need it.

  * Fixed: `energy_generation`'s dictionary-based PT/EN value recoding (`R/dictionary.R`, e.g. ANEEL's `"PCH"` code → `"pequena_central_hidreletrica"`/`"small_hydroelectric_center"`) was silently a no-op for the entire dataset. The dictionary's `~variable` column used short generic names (`"fonte"`, `"fase"`, `"origem"`, `"tipo"`, `"tipo_de_atuacao"`, `"combustivel_final"`, `"geracao_qualificada"`) that never matched any real column name after `janitor::clean_names()` (real names: `sig_tipo_geracao`, `dsc_fase_usina`, etc.) -- so every lookup silently matched zero rows and every raw ANEEL code passed straight through untranslated. Renamed all seven `~variable` groups to the real column identifiers, verified live against the current CSV with zero mismatches in either direction (one dictionary sentinel, `geracao_qualificada`'s `"-"`, doesn't match the live data's `""` for the same case -- left as-is, a separate pre-existing quirk). See `tests/testthat/test-aneel-schema.R`.

  * `load_aneel(dataset = "energy_generation")` (SIGA) moved off a dead GitLab-hosted xlsx (`git.aneel.gov.br/.../BD_SIGA.xlsx`, 403 Forbidden) onto ANEEL's own CKAN Open Data catalogue, which now serves the same dataset as a CSV (`siga-empreendimentos-geracao.csv`) -- same mechanism ANEEL's other two datasets already use (`resolve_aneel.R`'s CKAN `package_show` JSON, not HTML scraping), so `resolve_aneel.R` now resolves all three ANEEL datasets instead of two; `energy_generation`'s manifest row gained a real `resolver`/`url` where it used to be permanently hand-maintained. `R/download.R` reads it via `data.table::fread()` instead of `readxl::read_xlsx()` (`skip_rows`/`sheet` no longer apply to this dataset). Fixed alongside: the switch to CSV meant `mda_*`/`num_coord_*` power and coordinate columns, which the old xlsx stored as real Excel numerics, now arrive as Brazilian-locale strings (`"1400,00"`, `"-20,12479858"`) the same way `energy_enterprises_distributed`'s CSV always has -- the existing comma/thousand-separator cleaning step was scoped to `energy_enterprises_distributed` only and didn't cover `energy_generation`'s new shape, which would have silently left those columns as character. Generalized to run for both datasets (guarded on `is.character()`, since `fread()` can already auto-parse a comma-free column as numeric on its own). `aneel` excluded from the session download+parse cache (all-CSV now, nothing left to cache) and given its first `# FRAGILE:` tags. See `tests/testthat/test-aneel-schema.R`.

  * `resolve_seeg.R` stopped being detect-only, and `load_seeg()` was rewritten sector by sector to read SEEG's actual current file. Same story as IPS below: the resolver's detect-only status rested on an assumption ("the live file has a different shape") from when it was first written, never actually checked -- checked live 2026-09-15, and the real file (`seeg.eco.br`'s municipal summary workbook) turned out to be a full schema restructuring, not a version bump: the old `NIVEL 1`..`NIVEL 6` hierarchy is now `Setor de emissão`/`Categoria emissora`/`Sub-categoria emissora`/`Recorte`/`Atividade geral`, the year range widened from 2000-2018 to 1970-2024, and `GÁS`/`PRODUTO` are gone from this file entirely. All five `load_seeg()` sectors (`seeg_farming`, `seeg_industry`, `seeg_energy`, `seeg_residuals`, `seeg_land`) were migrated and verified live against the real ~250MB file (exact row counts, totals reconciled across municipality/state/country). Each sector lost some old granularity the new schema simply doesn't carry -- `produto`/`gas`, farming's species/crop-level detail, industry's technology-level detail, energy's fuel-type breakdown, and (the largest loss) `seeg_land`'s entire biome dimension and its full origin-to-destination land-cover transition matrix, collapsed into four coarser fields. Every loss is tagged `# FRAGILE:` in `R/seeg.R`, dropped rather than reconstructed or force-fit into an old bucket. `resolve_seeg.R` now emits a real `url`, verified live on every run via a ranged GET of the candidate xlsx's own `xl/workbook.xml` and `xl/pivotCache/pivotCacheDefinition1.xml` (an xlsx is a zip; reuses `zip_remote_listing.R`, which gained an optional `useragent` param since `seeg.eco.br` 403s any request without a browser-like one). `RESOLVER_ALERT_SOURCES` gained `"seeg"`. See `tests/testthat/test-seeg-*.R`.

  * `resolve_ips.R` stopped being detect-only (it used to emit only `version`/`docs_url`, never `url`, on an assumption -- that IPS Amazônia's Strapi-hosted workbook is "a different shape" than `R/ips.R`'s expected 4-year-sheet layout -- that was never actually checked). Checked live 2026-09-14: the two workbooks are a drop-in match (identical sheets, 772 rows/sheet, identical `janitor`-cleaned column names on all 4 year tabs), and the Strapi copy is the publisher's own file. (The manifest's old Google Drive link was confirmed owned by a personal gmail account, not Datazoom, and not in Datazoom's shared Drive folder -- a one-off personal upload that was never kept in sync with anything upstream.) All 8 IPS manifest rows now point at the real `painel.ipsamazonia.org.br` URL, re-verified every run via a ranged GET of the workbook's own `xl/workbook.xml` (an xlsx is a zip; reuses `zip_remote_listing.R`) rather than trusted from that one-time comparison. `load_ips()`'s hardcoded `sheet_list` (and the `"2018 "` trailing-space workaround baked into it) is gone -- years now come from the manifest's `available_time`, and a new `ips_match_sheets()` (`R/ips.R`) resolves a requested year to the workbook's own tab name by matching on the year digits, not the raw string, so a whitespace quirk (this one, or any future one) can't break `readxl::read_xlsx()`'s exact-string sheet lookup. `R/ips.R` also gets its first `# FRAGILE:` tags (the hardcoded `2012:2023` year-strip range, the `dplyr::contains()` substring column match, the `any_of()` silently-tolerant pt/eng rename map) via a new row in `fragility_notes.R`'s `FRAGILITY_SOURCES` table. See `tests/testthat/test-ips-sheets.R`.

  * Fixed: PRODES' `docs_url` (6 manifest rows) was pointing at a deforestation-rates dashboard app, not TerraBrasilis' actual downloads landing page (`https://terrabrasilis.dpi.inpe.br/downloads/`).

  * `load_mapbiomas` moved from a mix of GCS-bucket and WordPress-hosted links to MapBiomas's own Dataverse archive (`data.mapbiomas.org`) -- a documented REST API with permanent DOIs and versioned files, discovered to be genuinely ahead of both previous sources (fixes long-dead `"mapbiomas_mining"` and `"mapbiomas_water"` downloads, and picks up Collection 10/10.1 data neither previous source had mirrored). Added a new dataset, `"mapbiomas_secondary_vegetation"` -- Collection 10 split what used to be one combined deforestation+regeneration file into two separate ones; `"mapbiomas_deforestation_regeneration"` now covers only the deforestation half. Three geo_level rows are deliberately pinned to an older Dataverse collection than their siblings where a newer one is missing a needed sheet (most notably `"mapbiomas_mining"`'s indigenous-lands data, one collection behind its municipality-level sibling) -- not a bug, a discovered gap in what MapBiomas itself has republished.

  * Fixed `load_mapbiomas()`'s treatment code, which the Dataverse migration above changed the *links* for but not the *parsing* of -- Collection 10 didn't just re-host Collection 9's workbooks, it restructured several of them. Concretely this fixed: `"mapbiomas_cover"` (both geo_levels) and `"mapbiomas_water"`/`"biome"` hard-erroring (a `state`/`state_acronym` column collision on cover, a wide-shaped water/biome sheet the old code assumed was already long); `"mapbiomas_transition"`/`"biome"` hard-erroring (its window columns are now `p`-prefixed, e.g. `p1985_1986`, not `x`-prefixed); and `"mapbiomas_deforestation_regeneration"`'s municipality code silently not renaming (the source renamed `geocode` to `geocode_municipality`). Also fixed a related manifest bug: `"mapbiomas_cover"`/`"municipality"` was pointed at a Dataverse file (`Collection 10.1`) that has no municipality-code column in any sheet at all -- the resolver's own newest-collection-first walk now also verifies a candidate's columns, not just its sheet name, so it lands on Dataverse's plain `Collection 10` dataset instead (same file MapBiomas also serves off GCS) and will self-heal onto a newer collection automatically once one restores the column, no code change needed. See `R/mapbiomas.R`'s `mapbiomas_treat()` and `actions/scrapers/resolve_mapbiomas.R`'s `dv_file_layout()`.

  * `load_epe(dataset = "national_energy_balance")`'s source changed from EPE's old one-workbook-per-edition SharePoint file (2003-2023, one sheet per year, reconstructed into long format by hand) to EPE's own consolidated BEN table, which is already long-format and covers 1970-2025. **Breaking for `raw_data = TRUE`**: this now returns a list of one tibble instead of one tibble per year. Account (`conta`/`account`) labels also changed from a synthetic uppercase reconstruction (e.g. `"TRANSFORMACAO - REFINARIAS DE PETROLEO"`) to the source's own labels (e.g. `"Refinarias de Petróleo"`), and a new `tipo`/`type` column (Fontes de Energia Primária/Secundária/Total) is now available, previously implicit and unrecoverable from the old workbook's column headers.

  * `load_epe(dataset = "energy_state_panel")`'s source URL, found discontinued below, now has a replacement: EPE's BEN dashboard (the same book `national_energy_balance` reads) publishes a consolidated, already-tidy generation-by-source table on its Chapter 8 page (`tabela_geracao_eletricidade_por_fonte.xlsx`, long-format, 2011-2025, up from 2011-2024). The 16 `fonte` values it uses map 1:1 onto this dataset's existing columns; `R/epe.R` now reads it long and pivots it instead of parsing the old sheet's `"ANO BASE XXXX"` block structure. **Not breaking** for callers -- both raw and treated output keep the same shape and column names as before, including `raw_data = TRUE` (still one tibble). One cosmetic fix: 3 state names (`"Mato Grosso do Sul"`, `"Rio Grande do Sul"`, `"Rio Grande do Norte"`) are normalized to their standard lowercase connector, matching the old source's convention -- the new source capitalizes it (`"Mato Grosso Do Sul"`).
  * `load_epe(dataset = "energy_state_panel")`'s OLD source URL was found to be discontinued (EPE restructured its "Anuário Estatístico" from 8+ chapters to 4; no current publication has the same state x generation-source breakdown) -- see the fix directly above.

  * Refreshed the CI resolvers (`actions/scrapers/`) for PRODES, EPE and (new, detect-only) SEEG/IPS -- `resolve_prodes.R` and `resolve_epe.R` were previously documented no-ops. `build_manifest.R` gained a `--check-all` flag that reports every broken manifest URL (not just recently-changed ones) without failing the run, so long-standing breaks like the one above stay visible.

# datazoom.amazonia 1.2.0

  * Added `"energy_state_panel"` dataset to `load_epe`: yearly energy production by source and state (2011-2024), from EPE's BEN Chapter 8 (Dados Estaduais). 

  * Deprecated `load_datasus()`. All its datasets were moved to a new package: datazoom.saude. Beta version available.
  
  * Updated `load_mapbiomas` to the Collection 9 data, with both new and reformed datasets
  
  * Many small bug fixes

# datazoom.amazonia 1.1.5.9000 (development version)

  * Added support for four new SIH datasets in the load_datasus() function:
  "datasus_sih_rd" – Reduced AIHs (summary of hospitalizations)
  "datasus_sih_sp" – Professional Services performed during hospitalizations
  "datasus_sih_rj" – Rejected AIHs with general rejection reasons
  "datasus_sih_er" – Rejected AIHs with specific error codes
  
# datazoom.amazonia 1.1.4.9000 (development version)

  * `load_prodes` now reads the full raster data from PRODES

# datazoom.amazonia 1.1.3.9000 (development version)

  * Updated `download.R` to read the data now available at the EPE website.
  
  * Updated `load_epe` which has now 3 parameters following the split of `energy_consumption_per_class` in `industrial_energy_consumption` and `consumer_energy_consumption`. 
  
  * Added support for four new SIH datasets in the `load_datasus()` function
    
# datazoom.amazonia 1.1.2.9000 (development version)

  * Updated `load_mapbiomas` to download the newest Mapbiomas Collections and to warn about the currently unavailable download URL of the datasets water and irrigation. Also added the Indigenous Lands option to the `"mapbiomas_cover"` dataset.

# datazoom.amazonia 1.1.1.9000 (development version)

  * Updated `load_baci` to support the newest version of the data, fixing the previous broken download URL. (Thanks to @OlivazShai)

# datazoom.amazonia 1.1.0.9000 (development version)

  * Updated `load_prodes` data cleaning and download to allow more recent data

# datazoom.amazonia 1.1.0

  * Added a citation template for work using the package
  
  * Added the `"energy_enterprises_distributed"` dataset to `load_aneel`
  
  * Updated `load_mapbiomas` to the Collection 8 data, with both new and reformed datasets
  
  * Added the `load_population` function with Brazilian population data and estimates
  
  * Added the `load_censoagro` function with data from the Census of Agriculture
  
  * Updated `load_ips` to include 2023 data
  
  * Removed the `load_cipo` function
  
  * Many small bug fixes

# datazoom.amazonia 1.0.0

  * Adding new `load_epe` function for EPE data and adding new `load_aneel` function for ANEEL data
  
  * Changing the internal structure of `download.R`, which is behind all functions

# datazoom.amazonia 0.9.3.0

  * Making documentation consistent across functions

# datazoom.amazonia 0.9.2.9000

  * Fixing `load_climate` error when option `legal_amazon_only = TRUE`

# datazoom.amazonia 0.9.1.9000

  * Fixing `load_baci` timeout error upon download
  
# datazoom.amazonia 0.9.0.9000

  * Adding new `load_imazon` function for Imazon data

# datazoom.amazonia 0.8.5.9000

  * Updated collection 6 Mapbiomas data for "mapbiomas_transition" dataset

# datazoom.amazonia 0.8.4.9000

  * Exporting dataset with municipality codes and minor bug fixes

# datazoom.amazonia 0.8.3.9000

  * Code maintenance tweaks

# datazoom.amazonia 0.8.2.9000

  * Adding a sequential identification variable to `load_deter`

# datazoom.amazonia 0.8.1.9000

  * Changed code to initiate Deter download

# datazoom.amazonia 0.8.0.9000

  * Added new datasets to `load_ibama` for environmental distributed and collected fines

# datazoom.amazonia 0.7.2.9000

  * Fixing SSL verification error in download for `load_ibama`
  
# datazoom.amazonia 0.7.1.9000

  * Added municipalities code into `load_iema` function for energy data

# datazoom.amazonia 0.7.0.9000

  * Added new `load_iema` function for energy data

# datazoom.amazonia 0.6.0.9000

  * Added new `load_datasus` function for health data

# datazoom.amazonia 0.5.0.9000

  * Added new IPS datasets 

# datazoom.amazonia 0.4.0.9000

  * Added new Mapbiomas Mining dataset 

# datazoom.amazonia 0.3.0
  
## New functions
  
  * Plataforma CIPÓ data
  * TerraClimate data
  * BACI data for global trade
  
## Improvements
  
  * Documentation for `load_mapbiomas`
  * Bug fixes for PRODES, SEEG, and Ibama
  * Supports new IPS data format
  * Supports many more PAM datasets
  * No longer dependent on packages not published in CRAN

# datazoom.amazonia 0.2.0

* All functions supporting treated data download

# datazoom.amazonia 0.1.0

* All functions supporting raw data download

# datazoom.amazonia 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Added functions for PRODES data

# datazoom.amazonia 0.3.0.9000
* Added functions for GDP data 

# datazoom.amazonia 0.4.0.9000
* Added functions for COMEX data 

# datazoom.amazonia 0.5.0.9000
* Added functions for MAPBIOMAS data 

# datazoom.amazonia 0.6.0.9000
* Added functions for DEGRAD data 

# datazoom.amazonia 0.7.0.9000
* Added functions for DETER data 

# datazoom.amazonia 0.8.0.9000
* Added functions for SIGMINE data 

# datazoom.amazonia 0.9.0.9000
* Added functions for SIGMINE data 

# datazoom.amazonia 0.10.0.9000
* Added functions for IBGE - CEMPRE data
* Added functions for IBGE - Census data

# datazoom.amazonia 0.11.0.9000
* Added functions for IBGE - PAM data

# datazoom.amazonia 0.12.0.9000
* Added functions for IPS data 

# datazoom.amazonia 0.13.0.9000
* Added functions for SEEG data
