#' Convert columns to factors based on name patterns
#'
#' This function converts columns in a data frame to factors based on matching patterns
#' in their names. It can create either regular or ordered factors.
#'
#' @param dataf A data frame containing the columns to convert
#' @param patterns Character vector of patterns to match column names against
#' @param exclude Optional character vector of patterns to exclude from matching
#' @param ordered Logical indicating whether to create ordered factors. Default is FALSE
#' @param quiet Logical indicating whether to suppress messages. Default is FALSE
#'
#' @return A data frame with specified columns converted to factors
#'
#' @examples
#' testDf <- data.frame(
#'   cdx_var1 = c("A", "B", "A", "C"),
#'   cdx_var2 = c("X", "Y", "X", "Z"),
#'   age = c(25, 30, 22, 40),
#'   gender = c("M", "M", "F", "M")
#' )
#' newD <- convert_columns_to_factors(testDf, c("cdx", "gender"))
#' str(newD)
#'
#' @export
convert_columns_to_factors <- function(dataf, patterns, exclude = NULL, ordered = FALSE,
                                       quiet = FALSE) {
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("Input 'dataf' must be a data frame or tibble.")
  }
  if (!is.character(patterns) || length(patterns) == 0) {
    stop("'patterns' must be a non-empty character vector of column name patterns.")
  }
  
  # Combine patterns into a single regex pattern for matching
  combined_pattern <- paste(patterns, collapse = "|")
  
  # Find columns that match the pattern
  all_cols <- names(dataf)
  matching_cols <- grep(combined_pattern, all_cols, value = TRUE)
  
  # Apply exclusion patterns if provided
  if (!is.null(exclude) && length(exclude) > 0) {
    exclude_pattern <- paste(exclude, collapse = "|")
    matching_cols <- matching_cols[!grepl(exclude_pattern, matching_cols)]
  }
  
  # Check if any columns match after exclusions
  if (length(matching_cols) == 0) {
    warning("No columns found matching the patterns: ",
            paste(patterns, collapse = ", "),
            if (!is.null(exclude)) paste(" (after excluding: ",
                                         paste(exclude, collapse = ", "), ")", sep = ""))
    return(dataf)  # Return unchanged data if no match
  }
  
  # Create a result data frame
  result <- dataf
  
  # Perform the conversion for each matching column
  for (col in matching_cols) {
    x <- dataf[[col]]
    
    # Different conversion logic based on current column type
    if (is.factor(x)) {
      if (ordered && !is.ordered(x)) {
        # Convert regular factor to ordered
        result[[col]] <- factor(x, levels = levels(x), ordered = TRUE)
      }
      # If it's already the right kind of factor, do nothing
    } else {
      # Convert non-factor to factor
      result[[col]] <- factor(x, ordered = ordered)
    }
  }
  
  # Just the essential info (after conversion so we know it succeeded)
  if (!quiet) {
    message("Converted ", length(matching_cols), " columns to factors",
            if (length(matching_cols) <= 8) {
              paste0(": ", paste(matching_cols, collapse = ", "))
            } else {
              paste0(" (", paste(head(matching_cols, 5), collapse = ", "), " and ",
                     length(matching_cols) - 5, " more)")
            })
  }
  return(result)
}
