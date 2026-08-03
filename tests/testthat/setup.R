# Force every test to run fully offline against the packaged manifest snapshot.
# The remote manifest fetch (R/manifest.R) is only exercised in test-fallback.R,
# which flips this option back on and points it at an unreachable URL on purpose.

options(datazoom.amazonia.use_remote_manifest = FALSE)

withr::defer(
  {
    if (exists("clear_manifest_cache", envir = asNamespace("datazoom.amazonia"))) {
      datazoom.amazonia:::clear_manifest_cache()
    }
  },
  teardown_env()
)
