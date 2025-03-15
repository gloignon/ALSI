feature_registry <- list(
  token_proportions = list(
    func = function(features) {
      proportions <- features$parsed_corpus[, .N / .N[1], by = upos]
      result <- data.table(upos = proportions$upos, proportion = proportions$V1)
      return(list(token_proportions = result))
    },
    dependencies = c("parsed_corpus")
  ),
  
  lexical_features = list(
    func = function(features, lexical_db, db_name) {
      merged <- merge(features$parsed_corpus, lexical_db, by = "token", all.x = TRUE)
      result <- merged[, .(
        avg_frequency = mean(frequency, na.rm = TRUE),
        token_count = .N
      )]
      return(setNames(list(result), paste0("lexical_", db_name)))
    },
    dependencies = c("parsed_corpus")
  )
)

resolve_dependencies <- function(features, feature_name, params = list()) {
  if (!feature_name %in% names(feature_registry)) {
    stop(paste("Feature", feature_name, "is not in the registry."))
  }
  
  # Retrieve the dependencies
  deps <- feature_registry[[feature_name]]$dependencies
  if (!is.null(deps)) {
    for (dep in deps) {
      if (!dep %in% names(features)) {
        # Resolve missing dependency
        features <- resolve_dependencies(features, dep)
      }
    }
  }
  
  # Calculate the feature
  func <- feature_registry[[feature_name]]$func
  new_features <- do.call(func, c(list(features), params))
  
  # Aggregate the new feature into the features list
  features <- aggregate_features(features, new_features)
  return(features)
}

aggregate_features <- function(features, new_features) {
  for (name in names(new_features)) {
    if (!is.null(features[[name]])) {
      warning(paste("Feature", name, "already exists. Overwriting."))
    }
    features[[name]] <- new_features[[name]]
  }
  return(features)
}
