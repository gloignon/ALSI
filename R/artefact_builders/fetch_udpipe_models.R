# Fetch large UDPipe model files hosted as GitHub release assets.
#
# These models are too large to keep in regular git history (>50MB), so they
# are distributed as assets on the "models-v1" GitHub release instead. This
# script downloads any that are missing from the local models/ directory.
#
# Run from the repo root:
#   Rscript R/artefact_builders/fetch_udpipe_models.R

RELEASE_BASE <- "https://github.com/gloignon/ALSI/releases/download/models-v1"

models <- list(
  "french_gsd-remix_3.udpipe" = file.path(RELEASE_BASE, "french_gsd-remix_3.udpipe"),
  # ALSI-trained model on modified GSD (verb POS re-tagging) — pairs with the
  # distributed POS trigram model; used by demo_pos_surprisal.R.
  "fr_gsd_alsi_20260524_122826.udpipe" = file.path(RELEASE_BASE, "fr_gsd_alsi_20260524_122826.udpipe")
)

dir.create("models", showWarnings = FALSE, recursive = TRUE)

for (name in names(models)) {
  local_path <- file.path("models", name)
  if (file.exists(local_path)) {
    message("Already present: ", local_path)
    next
  }
  message("Downloading ", name, "...")
  download.file(models[[name]], local_path, mode = "wb", quiet = TRUE)
}
