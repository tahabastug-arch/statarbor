#' Format Effect Size Labels with Compact Scientific Symbols
#'
#' @param eff_list Named list of numeric effect size values.
#' @return Compact formatted character scalar.
#' @keywords internal
format_effect_compact <- function(eff_list) {
  lbl_map <- c(
    eta_squared = "\u03b7\u00b2",
    omega_squared = "omega^2",
    est_omega_squared = "est. omega^2",
    epsilon_squared = "\u03b5\u00b2",
    cohens_d = "d",
    r = "r"
  )
  nms <- names(eff_list)
  vals <- unlist(eff_list)
  pretty_nms <- ifelse(nms %in% names(lbl_map), lbl_map[nms], nms)
  paste(sprintf("%s = %.3f", pretty_nms, vals), collapse = ", ")
}

#' Academic ggplot2 Theme for statarbor Figures
#'
#' @param base_size Base font size in pt (default `12`).
#' @param base_family Font family (`"serif"` or `"sans"`).
#' @return A `ggplot2` theme object.
#' @keywords internal
theme_statarbor <- function(base_size = 12, base_family = "serif") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = "black",
                                               linewidth = 0.6),
      panel.grid.major = ggplot2::element_line(colour = "grey90", linewidth = 0.25),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = "black", size = ggplot2::rel(0.9)),
      axis.title = ggplot2::element_text(colour = "black", size = ggplot2::rel(1.0),
                                         face = "bold"),
      plot.title = ggplot2::element_text(colour = "black", size = ggplot2::rel(1.1),
                                         face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(colour = "grey25",
                                            size = ggplot2::rel(0.85), hjust = 0.5),
      legend.position = "none",
      plot.margin = ggplot2::margin(12, 12, 12, 12)
    )
}

#' Plot Method for statarbor_result Objects
#'
#' Creates a publication-ready boxplot or violin-boxplot annotated with
#' compact letter display (CLD) groups and summary test statistics.
#'
#' @param x An object of class `"statarbor_result"` returned by
#'   [auto_compare()].
#' @param type Plot geometry: `"boxplot"` (default) or `"violin"`.
#' @param show_letters Logical; if `TRUE` (default), places CLD group letters
#'   above each group.
#' @param font_family Font family: `"serif"` (default) or `"sans"`.
#' @param base_size Base font size in points (default `12`).
#' @param x_angle Rotation angle for x-axis group labels (default `45`).
#' @param title Optional custom plot title.
#' @param xlab Optional custom x-axis label.
#' @param ylab Optional custom y-axis label.
#' @param ... Unused; present for S3 generic compatibility.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   res <- auto_compare(iris, response = "Sepal.Length", group = "Species")
#'   plot(res, type = "boxplot")
#' }
plot.statarbor_result <- function(x,
                                  type = c("boxplot", "violin"),
                                  show_letters = TRUE,
                                  font_family = "serif",
                                  base_size = 12,
                                  x_angle = 45,
                                  title = NULL,
                                  xlab = NULL,
                                  ylab = NULL,
                                  ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }
  type <- match.arg(type)

  g_val <- y_val <- y_pos <- cld <- NULL
  df_plot <- x$data

  y_rng <- diff(range(df_plot$y_val, na.rm = TRUE))
  if (!is.finite(y_rng) || y_rng == 0) y_rng <- 1

  p_txt <- if (x$p_value < 0.001) "p < 0.001" else sprintf("p = %.4f", x$p_value)
  eff_txt <- format_effect_compact(x$effect_size)
  subtitle_txt <- sprintf("%s | %s = %.3f, %s | %s",
                          x$method, x$statistic_name, x$statistic, p_txt, eff_txt)

  n_grp <- nlevels(df_plot$g_val)
  base_pal <- c("#1B365D", "#D62728", "#2CA02C", "#FF7F0E",
                "#9467BD", "#8C564B", "#17BECF", "#7F7F7F")
  if (n_grp <= length(base_pal)) {
    pal <- base_pal[seq_len(n_grp)]
  } else if (requireNamespace("viridis", quietly = TRUE)) {
    pal <- viridis::viridis(n_grp, option = "D")
  } else {
    pal <- grDevices::hcl.colors(n_grp, palette = "Viridis")
  }

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = g_val, y = y_val, fill = g_val))

  if (type == "violin") {
    p <- p +
      ggplot2::geom_violin(alpha = 0.65, trim = TRUE, scale = "width",
                           color = "grey30", linewidth = 0.4) +
      ggplot2::geom_boxplot(width = 0.18, fill = "white", color = "black",
                            alpha = 0.9, outlier.size = 1.5)
  } else {
    p <- p +
      ggplot2::geom_boxplot(alpha = 0.80, color = "black",
                            outlier.colour = "#D62728", outlier.shape = 8,
                            outlier.size = 2)
  }

  p <- p +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(
      title = if (is.null(title)) paste0(x$response, " by ", x$group) else title,
      subtitle = subtitle_txt,
      x = if (is.null(xlab)) x$group else xlab,
      y = if (is.null(ylab)) x$response else ylab
    ) +
    theme_statarbor(base_size = base_size, base_family = font_family) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = x_angle,
        hjust = if (x_angle > 0) 1 else 0.5,
        vjust = 1
      )
    )

  if (isTRUE(show_letters) && !is.null(x$cld)) {
    grp_lvls <- levels(df_plot$g_val)
    max_vals <- vapply(grp_lvls, function(g) {
      max(df_plot$y_val[df_plot$g_val == g], na.rm = TRUE)
    }, numeric(1))

    letter_df <- data.frame(
      g_val = factor(grp_lvls, levels = grp_lvls),
      y_pos = max_vals + y_rng * 0.06,
      cld = x$cld$cld[match(grp_lvls, x$cld$group)],
      stringsAsFactors = FALSE
    )

    p <- p +
      ggplot2::geom_text(
        data = letter_df,
        ggplot2::aes(x = g_val, y = y_pos, label = cld),
        inherit.aes = FALSE,
        fontface = "bold",
        color = "#B91C1C",
        size = base_size / 2.8,
        family = font_family
      ) +
      ggplot2::expand_limits(y = max(letter_df$y_pos, na.rm = TRUE) + y_rng * 0.08)
  }

  p
}

#' Publication-Ready Correlation Heatmap
#'
#' Computes a pairwise correlation matrix across selected numeric columns
#' (automatically excluding zero-variance columns) and renders a lower-triangle
#' correlation heatmap.
#'
#' @param data A data frame.
#' @param vars Character vector of numeric column names (at least 2).
#' @param method Correlation method: `"pearson"` (default) or `"spearman"`.
#' @param font_family Font family (default `"serif"`).
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggcorrplot", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_correlation(iris, vars = names(iris)[1:4])
#' }
plot_correlation <- function(data,
                             vars,
                             method = c("pearson", "spearman"),
                             font_family = "serif") {
  if (!requireNamespace("ggcorrplot", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Packages 'ggcorrplot' and 'ggplot2' are required.", call. = FALSE)
  }
  method <- match.arg(method)

  sub <- data[, intersect(vars, names(data)), drop = FALSE]
  for (nm in names(sub)) {
    sub[[nm]] <- suppressWarnings(as.numeric(as.character(sub[[nm]])))
  }

  keep <- vapply(sub, function(v) {
    s <- stats::sd(v, na.rm = TRUE)
    !is.na(s) && s > 0
  }, logical(1))

  if (sum(keep) < 2L) {
    stop("At least 2 numeric variables with non-zero variance are required.",
         call. = FALSE)
  }

  cmat <- stats::cor(sub[, keep, drop = FALSE],
                     use = "pairwise.complete.obs",
                     method = method)

  title_txt <- if (method == "pearson") {
    "Correlation Matrix (Pearson r)"
  } else {
    "Correlation Matrix (Spearman rho)"
  }

  ggcorrplot::ggcorrplot(
    cmat,
    hc.order = TRUE,
    type = "lower",
    lab = TRUE,
    lab_size = 3.5,
    colors = c("#2563EB", "white", "#DC2626"),
    title = title_txt,
    ggtheme = theme_statarbor(base_size = 11, base_family = font_family)
  ) +
    ggplot2::theme(
      legend.position = "right",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

#' Principal Component Analysis (PCA) Biplot
#'
#' Performs centered and scaled Principal Component Analysis on selected
#' numeric variables and generates a grouped biplot via `factoextra`.
#'
#' @param data A data frame.
#' @param vars Character vector of numeric variable names (at least 2).
#' @param group Optional character scalar naming a grouping column for coloring
#'   observations.
#' @param add_ellipses Logical; whether to draw group concentration ellipses
#'   (default `FALSE`).
#' @param font_family Font family (default `"serif"`).
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' if (requireNamespace("factoextra", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_pca(iris, vars = names(iris)[1:4], group = "Species")
#' }
plot_pca <- function(data,
                     vars,
                     group = NULL,
                     add_ellipses = FALSE,
                     font_family = "serif") {
  if (!requireNamespace("factoextra", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Packages 'factoextra' and 'ggplot2' are required.", call. = FALSE)
  }

  cols_needed <- unique(c(vars, group))
  sub <- data[, intersect(cols_needed, names(data)), drop = FALSE]
  sub <- stats::na.omit(sub)

  num_part <- sub[, intersect(vars, names(sub)), drop = FALSE]
  for (nm in names(num_part)) {
    num_part[[nm]] <- suppressWarnings(as.numeric(as.character(num_part[[nm]])))
  }

  keep <- vapply(num_part, function(v) {
    s <- stats::sd(v, na.rm = TRUE)
    !is.na(s) && s > 0
  }, logical(1))

  if (sum(keep) < 2L || nrow(sub) < 5L) {
    stop("PCA requires at least 2 non-constant variables and 5 complete observations.",
         call. = FALSE)
  }

  pca_res <- stats::prcomp(num_part[, keep, drop = FALSE],
                           scale. = TRUE, center = TRUE)

  if (!is.null(group) && group %in% names(sub)) {
    p <- factoextra::fviz_pca_biplot(
      pca_res,
      geom.ind = "point",
      habillage = as.factor(as.character(sub[[group]])),
      addEllipses = isTRUE(add_ellipses),
      repel = TRUE,
      title = "Principal Component Analysis (PCA) Biplot"
    )
  } else {
    p <- factoextra::fviz_pca_biplot(
      pca_res,
      geom.ind = "point",
      repel = TRUE,
      title = "Principal Component Analysis (PCA) Biplot"
    )
  }

  p +
    theme_statarbor(base_size = 11, base_family = font_family) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 8),
      legend.title = ggplot2::element_text(size = 9, face = "bold"),
      legend.key.size = ggplot2::unit(0.35, "cm")
    )
}

#' Export Publication-Ready Word (.docx) Statistical Report
#'
#' Compiles one or more `"statarbor_result"` objects into a formatted Word
#' document containing an assumption-routing summary table, publication-ready
#' methods paragraphs, CLD-annotated descriptive tables, and embedded 300 DPI
#' figures.
#'
#' @param results Either a single `"statarbor_result"` object or a list of
#'   `"statarbor_result"` objects.
#' @param file Output `.docx` file path.
#' @param lang Report language: `"en"` (default) or `"tr"`.
#' @param plot_type Distribution geometry for embedded figures: `"boxplot"`
#'   (default) or `"violin"`.
#' @param font_family Font family for embedded figures (default `"serif"`).
#' @param title Optional document title.
#'
#' @return Invisibly returns the normalized `file` path.
#' @export
#'
#' @examples
#' if (requireNamespace("officer", quietly = TRUE) &&
#'     requireNamespace("flextable", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   res <- auto_compare(iris, response = "Sepal.Length", group = "Species")
#'   out_doc <- tempfile(fileext = ".docx")
#'   export_word_report(res, file = out_doc, lang = "en")
#'   file.exists(out_doc)
#' }
export_word_report <- function(results,
                               file,
                               lang = c("en", "tr"),
                               plot_type = c("boxplot", "violin"),
                               font_family = "serif",
                               title = NULL) {
  for (pkg in c("officer", "flextable", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required for export_word_report().", pkg),
           call. = FALSE)
    }
  }

  lang <- match.arg(lang)
  plot_type <- match.arg(plot_type)

  if (inherits(results, "statarbor_result")) {
    res_list <- list(results)
  } else if (is.list(results) && length(results) > 0L &&
             all(vapply(results, inherits, logical(1), "statarbor_result"))) {
    res_list <- results
  } else {
    stop("`results` must be a 'statarbor_result' object or a non-empty list of them.",
         call. = FALSE)
  }

  is_tr <- (lang == "tr")
  doc_title <- if (!is.null(title)) {
    title
  } else if (is_tr) {
    "statarbor - Otomatik \u0130statistiksel Analiz Raporu"
  } else {
    "statarbor - Automated Statistical Analysis Report"
  }

  sec1_hdr <- if (is_tr) "1. Otomatik Karar \u00d6zeti Tablosu" else "1. Decision Summary Table"
  sec2_hdr <- if (is_tr) "2. De\u011fi\u015fken Bazl\u0131 Sonu\u00e7lar ve Grafikler" else "2. Variable-Level Results and Figures"

  sum_rows <- lapply(res_list, function(r) {
    data.frame(
      Variable = r$response,
      Group = r$group,
      Method = r$method,
      Statistic = sprintf("%s = %.3f", r$statistic_name, r$statistic),
      p_value = if (r$p_value < 0.001) "< 0.001" else sprintf("%.4f", r$p_value),
      Effect_Size = format_effect_compact(r$effect_size),
      stringsAsFactors = FALSE
    )
  })
  sum_df <- do.call(rbind, sum_rows)

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, doc_title, style = "heading 1")
  doc <- officer::body_add_par(
    doc,
    paste0(if (is_tr) "Tarih: " else "Date: ", format(Sys.Date(), "%Y-%m-%d")),
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, sec1_hdr, style = "heading 2")

  ft_sum <- flextable::flextable(sum_df)
  ft_sum <- flextable::theme_booktabs(ft_sum)
  ft_sum <- flextable::fontsize(ft_sum, size = 8, part = "all")
  ft_sum <- flextable::padding(ft_sum, padding = 3, part = "all")
  ft_sum <- flextable::autofit(ft_sum)
  ft_sum <- flextable::fit_to_width(ft_sum, max_width = 6.3)
  doc <- flextable::body_add_flextable(doc, value = ft_sum)
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, sec2_hdr, style = "heading 2")

  for (r in res_list) {
    doc <- officer::body_add_par(
      doc,
      sprintf("%s (%s)", r$response, r$method),
      style = "heading 3"
    )
    doc <- officer::body_add_par(
      doc,
      generate_methods_text(r, lang = lang, include_citation = TRUE),
      style = "Normal"
    )
    doc <- officer::body_add_par(doc, "", style = "Normal")

    d <- r$descriptives
    tbl_out <- data.frame(
      Group = d$group,
      n = d$n,
      Mean_SD = sprintf("%.3f \u00b1 %s",
                        d$mean,
                        ifelse(is.na(d$sd), "0.000", sprintf("%.3f", d$sd))),
      Median_IQR = sprintf("%.3f (%.3f)", d$median, d$iqr),
      CLD = d$cld,
      stringsAsFactors = FALSE
    )
    ft_d <- flextable::flextable(tbl_out)
    ft_d <- flextable::theme_booktabs(ft_d)
    ft_d <- flextable::fontsize(ft_d, size = 8.5, part = "all")
    ft_d <- flextable::padding(ft_d, padding = 3, part = "all")
    ft_d <- flextable::autofit(ft_d)
    ft_d <- flextable::fit_to_width(ft_d, max_width = 6.3)
    doc <- flextable::body_add_flextable(doc, value = ft_d)
    doc <- officer::body_add_par(doc, "", style = "Normal")

    tmp_img <- tempfile(fileext = ".png")
    p_obj <- plot.statarbor_result(
      r,
      type = plot_type,
      show_letters = TRUE,
      font_family = font_family,
      base_size = 11
    )
    ggplot2::ggsave(tmp_img, plot = p_obj, width = 16, height = 11,
                    units = "cm", dpi = 300, bg = "white")
    doc <- officer::body_add_img(doc, src = tmp_img, width = 6.0, height = 4.1)
    doc <- officer::body_add_par(doc, "", style = "Normal")
    unlink(tmp_img)
  }

  print(doc, target = file)
  invisible(normalizePath(file, winslash = "/", mustWork = FALSE))
}
