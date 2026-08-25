# datazoom.amazonia (development version)

  * `load_mapbiomas` moved from a mix of GCS-bucket and WordPress-hosted links to MapBiomas's own Dataverse archive (`data.mapbiomas.org`) -- a documented REST API with permanent DOIs and versioned files, discovered to be genuinely ahead of both previous sources (fixes long-dead `"mapbiomas_mining"` and `"mapbiomas_water"` downloads, and picks up Collection 10/10.1 data neither previous source had mirrored). Added a new dataset, `"mapbiomas_secondary_vegetation"` -- Collection 10 split what used to be one combined deforestation+regeneration file into two separate ones; `"mapbiomas_deforestation_regeneration"` now covers only the deforestation half. Three geo_level rows are deliberately pinned to an older Dataverse collection than their siblings where a newer one is missing a needed sheet (most notably `"mapbiomas_mining"`'s indigenous-lands data, one collection behind its municipality-level sibling) -- not a bug, a discovered gap in what MapBiomas itself has republished.

  * Fixed `load_mapbiomas()`'s treatment code, which the Dataverse migration above changed the *links* for but not the *parsing* of -- Collection 10 didn't just re-host Collection 9's workbooks, it restructured several of them. Concretely this fixed: `"mapbiomas_cover"` (both geo_levels) and `"mapbiomas_water"`/`"biome"` hard-erroring (a `state`/`state_acronym` column collision on cover, a wide-shaped water/biome sheet the old code assumed was already long); `"mapbiomas_transition"`/`"biome"` hard-erroring (its window columns are now `p`-prefixed, e.g. `p1985_1986`, not `x`-prefixed); and `"mapbiomas_deforestation_regeneration"`'s municipality code silently not renaming (the source renamed `geocode` to `geocode_municipality`). Also fixed a related manifest bug: `"mapbiomas_cover"`/`"municipality"` was pointed at a Dataverse file (`Collection 10.1`) that has no municipality-code column in any sheet at all -- the resolver's own newest-collection-first walk now also verifies a candidate's columns, not just its sheet name, so it lands on Dataverse's plain `Collection 10` dataset instead (same file MapBiomas also serves off GCS) and will self-heal onto a newer collection automatically once one restores the column, no code change needed. See `R/mapbiomas.R`'s `mapbiomas_treat()` and `actions/scrapers/resolve_mapbiomas.R`'s `dv_file_layout()`.

  * Internal: `R/mapbiomas.R` now tags every shape-dependent assumption in its treatment code with a `# FRAGILE:` comment (substring matches, hardcoded exclusion lists, format-dependent regexes -- the kind of thing a future collection bump can silently violate without erroring). `actions/scripts/build_manifest.R` greps these live and lists the affected lines, by number, in the PR body whenever a scheduled run changes a MapBiomas manifest row -- so a reviewer knows exactly what to re-check instead of a generic "check R/*.R" reminder.

  * Internal: `resolve_mapbiomas.R`'s newest-collection-first walk now caches its own verified verdicts (`actions/cache/mapbiomas_resolver_cache.csv`, keyed on each candidate file's Dataverse checksum and a fingerprint of the current matching rules) so a scheduled run that finds nothing new skips re-downloading candidates it already checked, instead of re-verifying every file from scratch every week. A verdict is only ever trusted while both the file's content and the code's own `sheet_pattern`/`required_col_pattern` are provably unchanged since it was recorded. Caught two real bugs while building and verifying this against live Dataverse: the download step's timeout (120s) was far too short for a large candidate on a slow connection and silently misread a timeout as "this collection doesn't qualify," which cascaded the walk down to a much older, wrong candidate -- raised to 1000s (matching `R/download.R`'s own precedent) and, more importantly, a download failure (of any kind) now `stop()`s instead of returning "no match," so an infrastructure hiccup leaves that manifest row untouched rather than risking a silent downgrade to a worse candidate.

  * Internal: a site-link inventory (`actions/watch/site_links.csv`) now tracks every download link `brasil.mapbiomas.org`'s statistics page advertises, diffed on every scheduled manifest-refresh run and surfaced in the same pull request as any manifest change. Replaces an earlier design where the same page was scraped for a single freshness signal on one manifest row -- that design couldn't see a link disappearing, couldn't speak for any dataset besides `mapbiomas_cover`, and compared collection-number strings numerically in a way the source doesn't actually guarantee stays meaningful. See `actions/scripts/site_inventory.R` and the manifest-maintenance skill doc.

  * Internal: the manifest's per-dataset "base row" (the `geo_level`/`year`-blank row every dataset with real overrides used to also carry) was removed entirely. It was found live, stale: `"mapbiomas_mining"`'s base row stayed pinned to Collection 8 for weeks after both its real override rows had already moved to Collection 9 -- a base row could drift out of sync with its own overrides in exactly the way self-sufficient rows were supposed to prevent. `dataset_field()` now `stop()`s if a keyed dataset is queried without a real `geo_level`/`year` (previously silently answered from the base row); a new `dataset_meta()` is the one legal way to read a value that's genuinely the same across a whole dataset, and errors rather than guessing if two rows actually disagree. No exported function's signature changed; `datasets_link()`'s only visible effect is that 4 MapBiomas datasets whose geo_levels genuinely point at different files now report `url = NA` instead of one geo_level's value picked arbitrarily -- ask `load_mapbiomas()` for a specific `geo_level` instead. See `R/manifest.R`.

  * Internal: the hardcoded URL table inside `datasets_link()` was migrated to a versioned CSV manifest (`inst/extdata/manifest/v1/datasets_link.csv`), fetched at runtime with a silent fallback to the packaged snapshot when the network is unavailable. No exported function's signature or behavior changed; this only affects how source URLs are looked up internally, so that broken links (MapBiomas collection bumps, PRODES year rollovers, BACI version stamps, ANEEL CDE years) can be fixed by editing the manifest instead of waiting for a CRAN release.

  * Internal: the manifest schema was normalized to a 5-tier field-coalescing model (survey default -> dataset -> geo_level/year overrides), replacing the previous one-self-contained-row-per-override shape. `link`/`collection` were renamed `url`/`version`, and SIDRA rows' documentation landing page moved from `url` to a new `docs_url` column (`url` is genuinely absent for SIDRA-sourced datasets, which are downloaded via `sidra_code`, not a URL). No exported function changed; `datasets_link()` keeps its exact pre-existing column set and values. Filled manifest cells dropped from 1093 to 293 by letting a value live in exactly one place instead of being repeated across every row that needed it.

  * Internal: the 5-tier field-coalescing model above was replaced again, this time with fully self-sufficient rows and no inheritance at all -- `dataset_field()` now does a single exact-key lookup instead of a 5-tier walk, and a blank cell means "genuinely no value" rather than "inherit from the tier below". The 12 survey-default rows were removed, their values copied down onto every dataset row of that survey (filled cells rose from 293 to ~800). This closes out a real bug class the tiered model kept producing: a resolver and its own anti-duplication test fighting over whether an override row was allowed to state its own `version`; a base row (`mapbiomas_mining`, stuck at collection 8 while its overrides moved to 9) left silently stale because only its overrides were ever updated; `bind_rows()` NA-padding being misread as "clear this field". See `R/manifest.R` and `data-raw/denormalize_manifest.R`. No exported function changed.

  * Internal: the manifest-refresh GitHub Action (`update-manifest.yaml`) no longer auto-commits anything ("Tier A" changes). Every manifest change a resolver finds, however small, now opens a pull request for human review.

  * `load_epe(dataset = "national_energy_balance")`'s source changed from EPE's old one-workbook-per-edition SharePoint file (2003-2023, one sheet per year, reconstructed into long format by hand) to EPE's own consolidated BEN table, which is already long-format and covers 1970-2025. **Breaking for `raw_data = TRUE`**: this now returns a list of one tibble instead of one tibble per year. Account (`conta`/`account`) labels also changed from a synthetic uppercase reconstruction (e.g. `"TRANSFORMACAO - REFINARIAS DE PETROLEO"`) to the source's own labels (e.g. `"Refinarias de Petróleo"`), and a new `tipo`/`type` column (Fontes de Energia Primária/Secundária/Total) is now available, previously implicit and unrecoverable from the old workbook's column headers.

  * `load_epe(dataset = "energy_state_panel")`'s source URL was found to be discontinued (EPE restructured its "Anuário Estatístico" from 8+ chapters to 4; no current publication has the same state x generation-source breakdown). The dataset is left in place, pointing at a `docs_url` for anyone investigating a replacement, but downloads for it will keep failing until EPE republishes something equivalent or a maintainer picks a narrower substitute.

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
