#' Initialize a cache
#'
#' @param cache_name Name for the cache
#' @param cache_dir Directory for cache files (default: "cache")
#'
#' @return Cache object (list)
#' @export
initialize_cache <- function(cache_name, cache_dir = "cache") {
  cache_path <- file.path(cache_dir, paste0(cache_name, ".rds"))

  if (file.exists(cache_path)) {
    readRDS(cache_path)
  } else {
    list(
      name = cache_name,
      created = Sys.time(),
      entries = list()
    )
  }
}


#' Add entry to cache
#'
#' @param cache Cache object
#' @param key Key for the entry
#' @param value Value to cache
#'
#' @return Updated cache object
#' @export
add_to_cache <- function(cache, key, value) {
  cache$entries[[as.character(key)]] <- list(
    value = value,
    timestamp = Sys.time()
  )
  cache
}


#' Get entry from cache
#'
#' @param cache Cache object
#' @param key Key to retrieve
#' @param default Value to return if not found (default: NULL
#' @param max_age_days Maximum age in days (default: 30)
#'
#' @return Cached value or default
#' @export
get_from_cache <- function(cache, key, default = NULL, max_age_days = 30) {
  key <- as.character(key)
  entry <- cache$entries[[key]]

  if (is.null(entry)) return(default)

  # Check expiration
  age_days <- as.numeric(difftime(Sys.time(), entry$timestamp, units = "days"))
  if (age_days > max_age_days) return(default)

  entry$value
}


#' Save cache to disk
#'
#' @param cache Cache object
#' @param cache_name Name for the cache
#' @param cache_dir Directory for cache files
#' @param force Force save even if recently saved (default: TRUE)
#' @param periodic Unused, kept for compatibility
#'
#' @return Invisible NULL
#' @export
save_cache <- function(cache, cache_name, cache_dir = "cache",
                       force = TRUE, periodic = FALSE) {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_path <- file.path(cache_dir, paste0(cache_name, ".rds"))
  saveRDS(cache, cache_path)
  invisible(NULL)
}


#' Check if cache is expired
#'
#' @param cache Cache object
#' @param max_age_days Maximum age in days
#'
#' @return Logical
#' @export
is_cache_expired <- function(cache, max_age_days = 30) {
  if (is.null(cache$created)) return(TRUE)
  age_days <- as.numeric(difftime(Sys.time(), cache$created, units = "days"))
  age_days > max_age_days
}


#' Clean expired entries from cache
#'
#' @param cache Cache object
#' @param max_age_days Maximum age in days
#'
#' @return Cleaned cache object
#' @export
clean_cache <- function(cache, max_age_days = 30) {
  now <- Sys.time()

  cache$entries <- Filter(function(entry) {
    age_days <- as.numeric(difftime(now, entry$timestamp, units = "days"))
    age_days <= max_age_days
  }, cache$entries)

  cache
}


#' Get cache statistics
#'
#' @param cache Cache object
#'
#' @return List with entry_count and age
#' @export
get_cache_stats <- function(cache) {
  list(
    entry_count = length(cache$entries),
    created = cache$created,
    age_days = as.numeric(difftime(Sys.time(), cache$created, units = "days"))
  )
}
