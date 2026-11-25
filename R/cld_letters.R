#' Generate Special-Character-Safe Compact Letter Display (CLD)
#'
#' Constructs compact letter display (CLD) groups from pairwise post-hoc
#' adjusted p-values using a symmetric logical difference matrix. Unlike
#' string-splitting approaches, this implementation safely handles hyphens,
#' degree symbols, and spaces in group names (e.g., `"-196°C - 1. Week"`).
#'
#' @param posthoc_df A data frame containing pairwise comparison results with
#'   columns `group1`, `group2`, and `p.adj` (as returned by `rstatix`).
#' @param ordered_groups Character vector of unique group labels ordered from
#'   highest to lowest summary metric so that `"a"` is assigned to the top group.
#' @param alpha Numeric significance threshold (default `0.05`).
#'
#' @return A data frame with columns `group` and `cld`.
#' @export
#'
#' @examples
#' ph <- data.frame(
#'   group1 = c("Control", "Control", "-20°C - 1W"),
#'   group2 = c("-20°C - 1W", "-196°C - 1W", "-196°C - 1W"),
#'   p.adj  = c(0.001, 0.0001, 0.85)
#' )
#' generate_cld(ph, c("Control", "-20°C - 1W", "-196°C - 1W"))
generate_cld <- function(posthoc_df, ordered_groups, alpha = 0.05) {
  ordered_groups <- as.character(unique(ordered_groups))
  k <- length(ordered_groups)

  if (k < 2L) {
    return(data.frame(group = ordered_groups, cld = rep("a", k),
                      stringsAsFactors = FALSE))
  }

  diff_mat <- matrix(
    FALSE,
    nrow = k,
    ncol = k,
    dimnames = list(ordered_groups, ordered_groups)
  )

  if (!is.null(posthoc_df) && nrow(posthoc_df) > 0L) {
    req_cols <- c("group1", "group2", "p.adj")
    if (!all(req_cols %in% names(posthoc_df))) {
      stop("`posthoc_df` must contain columns: 'group1', 'group2', and 'p.adj'.",
           call. = FALSE)
    }

    for (i in seq_len(nrow(posthoc_df))) {
      g1 <- as.character(posthoc_df$group1[i])
      g2 <- as.character(posthoc_df$group2[i])
      pval <- posthoc_df$p.adj[i]

      if (!is.na(pval) && pval < alpha &&
          g1 %in% ordered_groups && g2 %in% ordered_groups) {
        diff_mat[g1, g2] <- TRUE
        diff_mat[g2, g1] <- TRUE
      }
    }
  }

  diag(diff_mat) <- FALSE

  if (!any(diff_mat)) {
    return(data.frame(group = ordered_groups, cld = rep("a", k),
                      stringsAsFactors = FALSE))
  }

  letters_vec <- tryCatch({
    res <- multcompView::multcompLetters(diff_mat, threshold = alpha)
    vec <- if (is.list(res)) res$Letters else res
    vec[ordered_groups]
  }, error = function(e) {
    stats::setNames(rep("a", k), ordered_groups)
  })

  data.frame(
    group = ordered_groups,
    cld = as.character(letters_vec),
    stringsAsFactors = FALSE
  )
}
