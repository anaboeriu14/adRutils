
#' Generate a cache file path
#'
#' Creates a standardized path for cache files, ensuring valid filenames and proper directory structure.
#'
#' @param cache_name Character string for the base name of the cache file
#' @param cache_dir Character string for the directory where cache files are stored
#' @param extension Character string for the file extension (should start with '.')
#' @param create_dir Logical indicating whether to create the cache directory if it doesn't exist
#' @param validate_name Logical indicating whether to validate and sanitize the cache name
#'
#' @returns Character string containing the full path to the cache file
#'
#' @examples
#' \dontrun{
#' # Get a path for medication cache
#' cache_path <- get_cache_path("medication_lookup", "cache", ".rds")
#' }
#'
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

#' Initialize a cache
#'
#' Loads an existing cache file or creates a new one if it doesn't exist.
#'
#' @param cache_name Character string for the base name of the cache file
#' @param cache_dir Character string for the directory where cache files are stored
#' @param extension Character string for the file extension
#'
#' @returns A list containing the cache data
#' @export
initialize_cache <- function(cache_name, cache_dir = "cache", extension = ".rds") {
  cache_path <- get_cache_path(cache_name, cache_dir, extension)

  if (file.exists(cache_path)) {
    message("Loading existing cache from: ", cache_path)
    return(readRDS(cache_path))
  } else {
    message("No existing cache found. Creating new cache.")
    return(list(
      created = Sys.time(),
      last_updated = Sys.time(),
      entries = list()
    ))
  }
}

#' Save cache to disk
#'
#' Saves a cache object to disk, with options for periodic saving and compression.
#'
#' @param cache List containing the cache data
#' @param cache_name Character string for the base name of the cache file
#' @param cache_dir Character string for the directory where cache files are stored
#' @param extension Character string for the file extension
#' @param force Logical indicating whether to force saving regardless of other conditions
#' @param periodic Logical indicating whether to use periodic saving based on counter
#' @param save_frequency Integer indicating how often to save when using periodic saving
#' @param counter Integer indicating the current iteration count
#' @param compress Logical indicating whether to use compression when saving, default = FALSE
#'
#' @returns Logical indicating whether the cache was saved
#' @export
save_cache <- function(cache, cache_name, cache_dir = "cache", extension = ".rds",
                       force = FALSE, periodic = TRUE,
                       save_frequency = 100, counter = NULL,
                       compress = FALSE) {

  cache_path <- get_cache_path(cache_name, cache_dir, extension)

  # Check if we should save: either force = T or periodic condition met
  should_save <- force || (periodic && !is.null(counter) && counter %% save_frequency == 0)

  if (should_save) {
    # Update timestamp
    cache$last_updated <- Sys.time()

    # Ensure the directory exists before saving
    dir_path <- dirname(cache_path)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      message("Created cache directory: ", dir_path)
    }

    # Use compression if requested
    if (compress) {
      saveRDS(cache, cache_path)
    } else {
      saveRDS(cache, cache_path, compress = "xz")

    }

    if (!is.null(counter)) {
      message("Cache saved at iteration ", counter)
    } else {
      message("Cache saved")
    }
    return(TRUE)
  }
  return(FALSE)
}

#' Retrieve a value from the cache
#'
#' Gets a value from the cache by key, with optional expiration checking.
#'
#' @param cache List containing the cache data
#' @param key Character string for the cache key
#' @param default Value to return if the key is not found or if the entry is expired
#' @param max_age_days Numeric indicating the maximum age in days for cached entries, NULL for no expiration
#'
#' @returns The cached value or the default value if not found or expired
#'
#' @export
get_from_cache <- function(cache, key, default = NULL, max_age_days = NULL) {
  if (!("entries" %in% names(cache)) || !is.list(cache$entries)) {
    return(default)
  }

  if (!(key %in% names(cache$entries))) {
    return(default)
  }

  entry <- cache$entries[[key]]

  # Check if entry exists and has timestamp
  if (!is.list(entry) || !("timestamp" %in% names(entry)) || !("value" %in% names(entry))) {
    return(default)
  }

  # Check expiration
  if (!is.null(max_age_days)) {
    age <- difftime(Sys.time(), entry$timestamp, units = "days")
    if (age > max_age_days) {
      return(default)  # Entry expired
    }
  }

  return(entry$value)
}

#' Store a value in the cache
#'
#' Adds or updates a value in the cache with a timestamp.
#'
#' @param cache List containing the cache data
#' @param key Character string for the cache key
#' @param value The value to store in the cache
#'
#' @returns The updated cache list
#' @export
add_to_cache <- function(cache, key, value) {
  # Ensure cache has entries list
  if (!("entries" %in% names(cache)) || !is.list(cache$entries)) {
    cache$entries <- list()
  }

  # Store value with timestamp
  cache$entries[[key]] <- list(
    value = value,
    timestamp = Sys.time()
  )
  return(cache)
}

#' Check if a cache is expired
#'
#' Determines if a cache is expired based on its last update timestamp.
#'
#' @param cache List containing the cache data
#' @param max_age_days Numeric indicating the maximum age in days for the cache
#'
#' @returns Logical indicating whether the cache is expired
#' @export
is_cache_expired <- function(cache, max_age_days = 30) {
  if (is.null(cache$last_updated)) {
    return(TRUE)  # No timestamp, consider expired
  }

  age <- difftime(Sys.time(), cache$last_updated, units = "days")
  return(age > max_age_days)
}

#' Clean expired entries from a cache
#'
#' Removes entries from a cache that are older than the specified maximum age.
#'
#' @param cache List containing the cache data
#' @param max_age_days Numeric indicating the maximum age in days for cached entries
#'
#' @returns The updated cache list with expired entries removed
#'
#' @export
clean_cache <- function(cache, max_age_days = 30) {
  if (!("entries" %in% names(cache)) || !is.list(cache$entries) || length(cache$entries) == 0) {
    return(cache)
  }

  # Find expired entries
  current_time <- Sys.time()
  expired_keys <- character(0)

  for (key in names(cache$entries)) {
    entry <- cache$entries[[key]]
    if (is.list(entry) && "timestamp" %in% names(entry)) {
      age <- difftime(current_time, entry$timestamp, units = "days")
      if (age > max_age_days) {
        expired_keys <- c(expired_keys, key)
      }
    }
  }

  # Remove expired entries
  if (length(expired_keys) > 0) {
    message("Removing ", length(expired_keys), " expired cache entries")
    cache$entries[expired_keys] <- NULL
  }

  return(cache)
}

#' Get cache statistics
#'
#' Provides statistics about the cache contents.
#'
#' @param cache List containing the cache data
#'
#' @returns A list with statistics about the cache
#' @export
get_cache_stats <- function(cache) {
  entry_count <- 0
  if (!is.null(cache$entries)) {
    entry_count <- length(cache$entries)
  }

  created <- NA
  last_updated <- NA
  age_days <- NA

  if (!is.null(cache$created)) {
    created <- cache$created
  }

  if (!is.null(cache$last_updated)) {
    last_updated <- cache$last_updated
    age_days <- as.numeric(difftime(Sys.time(), last_updated, units = "days"))
  }

  return(list(
    entry_count = entry_count,
    created = created,
    last_updated = last_updated,
    age_days = age_days,
    size_estimate = utils::object.size(cache)
  ))
}

#' Remove items from cache by regular expression
#'
#' Removes items from the cache whose keys match a regular expression pattern.
#'
#' @param cache List containing the cache data
#' @param pattern Regular expression pattern to match against cache keys
#'
#' @returns The updated cache list with matching entries removed
#'
#'
#' @export
remove_from_cache_by_pattern <- function(cache, pattern) {
  if (!("entries" %in% names(cache)) || !is.list(cache$entries)) {
    return(cache)
  }

  matches <- grep(pattern, names(cache$entries), value = TRUE)
  if (length(matches) > 0) {
    message("Removing ", length(matches), " entries matching pattern '", pattern, "'")
    cache$entries[matches] <- NULL
  }

  return(cache)
}

#' Export a cache to different formats
#'
#' Exports cache contents to different formats like CSV, or data frame.
#'
#' @param cache List containing the cache data
#' @param format Character string for the export format ("csv", "json", "df")
#' @param file_path Character string for the path to save the exported file (optional)
#'
#' @returns Depends on format: data frame for "df", NULL for others (writes to file)
#' @export
export_cache <- function(cache, format = c("csv", "dataframe"), file_path = NULL) {
  format <- match.arg(format)

  if (!("entries" %in% names(cache)) || !is.list(cache$entries)) {
    warning("Cache has no entries to export")
    return(data.frame())
  }

  # Convert to data frame
  keys <- names(cache$entries)
  values <- character(length(keys))
  timestamps <- character(length(keys))

  for (i in seq_along(keys)) {
    entry <- cache$entries[[keys[i]]]
    if (is.list(entry) && "value" %in% names(entry)) {
      values[i] <- if(is.character(entry$value)) entry$value else as.character(entry$value)
      timestamps[i] <- if("timestamp" %in% names(entry)) as.character(entry$timestamp) else NA
    }
  }

  # Create data frame
  cache_df <- data.frame(
    key = keys,
    value = values,
    timestamp = timestamps,
    stringsAsFactors = FALSE
  )

  # Export based on format
  if (format == "csv") {
    if (is.null(file_path)) {
      stop("file_path must be provided when format='csv'")
    }
    utils::write.csv(cache_df, file = file_path, row.names = FALSE)
    message("Exported cache to CSV: ", file_path)
    return(invisible(NULL))
  } else if (format == "dataframe") {
    return(cache_df)
  }
}
