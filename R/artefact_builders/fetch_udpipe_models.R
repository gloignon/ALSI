# Fetch large UDPipe model files hosted as GitHub release assets.
#
# These models are too large to keep in regular git history (>50MB), so they
# are distributed as assets on the "models-v1" GitHub release instead. This is
# a thin batch wrapper over ensure_udpipe_model() in R/fnt_setup.R, which holds
# the release URL and asset list; it downloads any model still missing from the
# local models/ directory.
#
# Run from the repo root:
#   Rscript R/artefact_builders/fetch_udpipe_models.R

source("R/fnt_setup.R", encoding = "UTF-8")

for (name in .alsi_udpipe_models) {
  ensure_udpipe_model(name)
}
