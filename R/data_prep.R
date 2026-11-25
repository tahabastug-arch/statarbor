#' Merge Group Labels Sharing a Common Prefix
#'
#' Collapses numbered or sub-indexed experimental labels that share a common
#' prefix (e.g., `"Control 1"`, `"Control 2"`, ..., `"Control 63"`) into a
#' single unified group label (e.g., `"Control"`). Useful for cleaning raw
#' laboratory spreadsheets prior to group comparisons.
#'
#' @param data A data frame.
#' @param column Character scalar naming the column to clean.
#' @param prefix Character scalar specifying the leading text prefix to match.
#' @param new_label Replacement label for matched entries (defaults to `prefix`).
#' @param new_col Name of the output column (defaults to `paste0(column, "_clean")`).
#'   Set `new_col = column` to overwrite the column in place.
#' @param ignore_case Logical; if `TRUE`, prefix matching is case-insensitive
#'   (default `FALSE`).
#'
#' @return A copy of `data` with the cleaned grouping column added (or updated).
#' @export
#'
#' @examples
#' df <- data.frame(
#'   grp = c("Control 1", "Control 2", "Control 3", "Treat_A", "Treat_A"),
#'   val = c(10, 12, 11, 5, 6)
#' )
#' merge_group_prefix(df, column = "grp", prefix = "Control")
merge_group_prefix <- function(data,
                               column,
                               prefix,
                               new_label = prefix,
                               new_col = paste0(column, "_clean"),
                               ignore_case = FALSE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!column %in% names(data)) {
    stop(sprintf("Column '%s' not found in `data`.", column), call. = FALSE)
  }
  if (!is.character(prefix) || length(prefix) != 1L || !nzchar(trimws(prefix))) {
    stop("`prefix` must be a non-empty character string.", call. = FALSE)
  }

  vals <- as.character(data[[column]])
  trimmed_vals <- trimws(vals)
  trimmed_prefix <- trimws(prefix)

  if (isTRUE(ignore_case)) {
    matched <- !is.na(trimmed_vals) &
      startsWith(tolower(trimmed_vals), tolower(trimmed_prefix))
  } else {
    matched <- !is.na(trimmed_vals) & startsWith(trimmed_vals, trimmed_prefix)
  }

  cleaned <- vals
  cleaned[matched] <- new_label

  out <- data
  lvls <- unique(cleaned[!is.na(cleaned)])
  out[[new_col]] <- factor(cleaned, levels = lvls)
  out
}

#' Combine Two or More Factor Columns into an Interaction Label
#'
#' Creates a composite grouping factor by concatenating two or more columns
#' (e.g., storage temperature and duration) with a user-defined separator.
#' Rows containing `NA` in any selected column receive `NA` in the combined
#' column.
#'
#' @param data A data frame.
#' @param cols Character vector of at least 2 column names to combine.
#' @param sep Character separator placed between factor values (default `" - "`).
#' @param new_col Name of the new combined column (defaults to
#'   `paste(cols, collapse = "_x_")`).
#'
#' @return A copy of `data` with the new combined factor column appended.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   temp = c("4C", "4C", "0C", "0C"),
#'   week = c("1W", "2W", "1W", "2W")
#' )
#' combine_factors(df, cols = c("temp", "week"))
combine_factors <- function(data,
                            cols,
                            sep = " - ",
                            new_col = paste(cols, collapse = "_x_")) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(cols) || length(cols) < 2L) {
    stop("`cols` must specify at least 2 column names.", call. = FALSE)
  }
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0L) {
    stop(sprintf("Column(s) not found in `data`: %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  sub_list <- lapply(cols, function(cn) as.character(data[[cn]]))
  has_na <- Reduce(`|`, lapply(sub_list, is.na))

  combined <- do.call(paste, c(sub_list, list(sep = sep)))
  combined[has_na] <- NA_character_

  out <- data
  lvls <- unique(combined[!is.na(combined)])
  out[[new_col]] <- factor(combined, levels = lvls)
  out
}
