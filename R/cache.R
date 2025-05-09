#' Create a standardized cache file path
#'
#' @param cache_name Character string for the cache name (will be sanitized)
#' @param cache_dir Directory to store cache files, defaults to "cache"
#' @param extension File extension to use, defaults to ".rds"
#' @param create_dir Logical, should the directory be created if missing?
#' @param validate_name Logical, should cache_name be validated/sanitized?
#' @return A full path to the cache file
#' @export

get_cache_path <- function(cache_name, cache_dir = "cache", extension = ".rds",
                           create_dir = TRUE, validate_name = TRUE) {

  # Input validation
  if (!is.character(cache_name) || length(cache_name) != 1 || nchar(cache_name) == 0) {
    stop("cache_name must be a non-empty character string")
  }

  if (!is.character(cache_dir) || length(cache_dir) != 1) {
    stop("cache_dir must be a character string")
  }

  # Ensure extension starts with a dot
  if (!grepl("^\\.", extension)) {
    extension <- paste0(".", extension)
  }

  # Validate cache name if requested
  if (validate_name) {
    # Replace invalid characters with underscores
    original_name <- cache_name
    cache_name <- gsub("[^a-zA-Z0-9_\\-\\.]", "_", cache_name)

    if (cache_name != original_name) {
      warning(sprintf("Cache name contained invalid characters and was sanitized: '%s' -> '%s'",
                      original_name, cache_name))
    }
  }

  # Create full cache directory path using here for project-relative paths
  full_cache_dir <- here::here(cache_dir)

  # Create directory if it doesn't exist and create_dir is TRUE
  if (create_dir && !dir.exists(full_cache_dir)) {
    dir.create(full_cache_dir, recursive = TRUE)
    message(sprintf("Created cache directory: %s", full_cache_dir))
  }

  # Generate full cache path
  cache_path <- file.path(full_cache_dir, paste0(cache_name, extension))

  return(cache_path)
}

#' Initialize or load a cache
#'
#' This function creates a new cache or loads an existing one from disk. If the cache doesn't exist, it will return an empty list. If it does
#' exist it will load the cache from the file.
#'
#' @param cache_name  Name of the cache
#' @param cache_dir Directory where cache files are stored, defaults to "cache"
#' @param extension  File extension to use, defaults to ".rds"
#'
#' @returns A list containing the cache data
#' @export
initialize_cache <- function(cache_name, cache_dir = "cache", extension = ".rds") {
  # Construct full path using here()
  cache_path <- get_cache_path(cache_name, cache_dir, extension)
  # Load existing cache or create new one
  if (file.exists(cache_path)) {
    message("Loading existing cache from: ", cache_path)
    return(readRDS(cache_path))
  } else {
    message("No existing cache found. Creating new cache.")
    return(list())
  }
}

#' Save cache to disk
#'
#' This function saves a cache to disk, either immediately or
#' based on a periodic counter. This allows for periodic saving during
#' long-running operations to prevent data loss.
#'
#' @param cache The cache object to save
#' @param cache_name Name of the cache
#' @param cache_dir Directory where cache files are stored, defaults to "cache"
#' @param extension File extension to use, defaults to ".rds"
#' @param force Logical, force saving regardless of counter
#' @param periodic Logical, enable periodic saving based on counter
#' @param save_frequency How often to save when periodic is TRUE, default is 100
#' @param counter Current iteration counter for periodic saving
#'
#' @return Logical, TRUE if cache was saved, FALSE otherwise
#' @export
save_cache <- function(cache, cache_name, cache_dir = "cache", extension = ".rds",
                       force = FALSE, periodic = TRUE,
                       save_frequency = 100, counter = NULL) {

  cache_path <- get_cache_path(cache_name, cache_dir, extension)
  # Check if we should save: either force = T or periodic condition met
  should_save <- force || (periodic && !is.null(counter) && counter %% save_frequency == 0)
  if (should_save) {
    # Ensure the directory exists before saving
    dir_path <- dirname(cache_path)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      message("Created cache directory: ", dir_path)
    }
    saveRDS(cache, cache_path)
    if (!is.null(counter)) {
      message("Cache saved at iteration ", counter)
    } else {
      message("Cache saved")
    }
    return(TRUE)
  }
  return(FALSE)
}

#' Retrieve a value from cache
#'
#' This function safely retrieves a value from a cache object
#' by key, returning a default value if the key doesn't exist.
#'
#' @param cache The cache object (list) to retrieve from
#' @param key The key to look up in the cache
#' @param default The default value to return if key is not found
#'
#' @return The cached value or the default if not found
#' @export
get_from_cache <- function(cache, key, default = NULL) {
  if (key %in% names(cache)) {
    return(cache[[key]])
  } else {
    return(default)
  }
}


#' Add an entry to the cache
#'
#' This function adds or updates a key-value pair in the cache.
#'
#' @param cache The cache object (list) to add to
#' @param key The key to store the value under
#' @param value The value to store
#'
#' @return The updated cache object
#' @export
add_to_cache <- function(cache, key, value) {
  cache[[key]] <- value
  return(cache)
}

#' Find items that need to be fetched (not in cache)
#'
#' This function compares a list of items against the cache
#' to determine which ones need to be fetched. This is useful
#' for batch processing and API calls.
#'
#' @param items Vector of items to check against the cache
#' @param cache The cache object (list) to check
#'
#' @return Vector of items not found in the cache
#' @export
get_items_to_fetch <- function(items, cache) {
  cache_keys <- names(cache)
  items_to_fetch <- items[!as.character(items) %in% cache_keys]

  message(paste0("Found ", length(items) - length(items_to_fetch),
                 " items in cache, need to fetch ", length(items_to_fetch)))

  return(items_to_fetch)
}
