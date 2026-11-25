# Bilingual (EN/TR) Shiny Module for statarbor v0.2.0 Experimental Designs
# Covers: compare_paired(), auto_repeated(), analyze_count(),
#         analyze_ancova(), and analyze_rcbd()

.v02_dict <- list(
  sidebar_hdr = list(en = "Experimental Design Setup", tr = "Deneme Deseni Ayarlar\u0131"),
  design_lbl  = list(en = "Analysis Module:", tr = "Analiz Mod\u00fcl\u00fc:"),
  use_demo    = list(en = "Use built-in demo data for this design", tr = "Bu desen i\u00e7in haz\u0131r demo verisi kullan"),
  resp_lbl    = list(en = "Numeric Response:", tr = "Say\u0131sal Yan\u0131t De\u011fi\u015fkeni:"),
  cond_lbl    = list(en = "Condition / Time Factor:", tr = "Ko\u015ful / Zaman Fakt\u00f6r\u00fc:"),
  id_lbl      = list(en = "Subject / Replicate ID:", tr = "Denek / Tekerr\u00fcr Kimli\u011fi (ID):"),
  grp_lbl     = list(en = "Grouping / Treatment Factor:", tr = "Grup / Muamele Fakt\u00f6r\u00fc:"),
  cov_lbl     = list(en = "Continuous Covariate:", tr = "S\u00fcrekli Kovaryat:"),
  blk_lbl     = list(en = "Blocking Factor:", tr = "Blok Fakt\u00f6r\u00fc:"),
  off_lbl     = list(en = "Exposure Offset (optional):", tr = "Maruziyet Ofseti (opsiyonel):"),
  mode_lbl    = list(en = "Routing Mode:", tr = "Karar Stratejisi:"),
  fam_lbl     = list(en = "Count Model Family:", tr = "Say\u0131m Modeli Ailesi:"),
  blk_type    = list(en = "Block Effect Type:", tr = "Blok Etkisi T\u00fcr\u00fc:"),
  padj_lbl    = list(en = "Post-Hoc P-Adjustment:", tr = "Post-Hoc P D\u00fczeltmesi:"),
  homog_lbl   = list(en = "Auto-switch on heterogeneous slopes", tr = "E\u011fim heterojenli\u011finde etkile\u015fimli modele ge\u00e7"),
  alpha_lbl   = list(en = "Significance Level (\u03b1):", tr = "Anlaml\u0131l\u0131k D\u00fczeyi (\u03b1):"),
  style_lbl   = list(en = "Plot Style:", tr = "Grafik Stili:"),
  run_btn     = list(en = "Run Design Analysis", tr = "Desen Analizini \u00c7al\u0131\u015ft\u0131r"),
  tab_sum     = list(en = "Summary & Descriptives", tr = "\u00d6zet ve Tan\u0131mlay\u0131c\u0131lar"),
  tab_pw      = list(en = "Pairwise Comparisons", tr = "\u0130kili Kar\u015f\u0131la\u015ft\u0131rmalar"),
  tab_plt     = list(en = "CLD Plot", tr = "CLD Grafi\u011fi"),
  tab_rep     = list(en = "Methods Text & R Code", tr = "Y\u00f6ntem Metni ve R Kodu"),
  dl_png      = list(en = "Download Plot (300 DPI PNG)", tr = "Grafi\u011fi \u0130ndir (300 DPI PNG)"),
  dl_r        = list(en = "Download R Script (.R)", tr = "R Beti\u011fini \u0130ndir (.R)"),
  no_pw       = list(en = "Two-condition paired tests have a single omnibus comparison (no pairwise table needed).",
                     tr = "\u0130ki ko\u015fullu e\u015fle\u015ftirilmi\u015f testlerde tek genel kar\u015f\u0131la\u015ft\u0131rma yap\u0131l\u0131r (ikili tablo gerekmez)."),
  run_hint    = list(en = "Select variables and click 'Run Design Analysis' to compute results.",
                     tr = "De\u011fi\u015fkenleri se\u00e7ip 'Desen Analizini \u00c7al\u0131\u015ft\u0131r' butonuna t\u0131klay\u0131n.")
)

.v02_demo_data <- function(design) {
  set.seed(42)
  if (design == "paired") {
    n <- 18
    base_y <- rnorm(n, 12, 2)
    data.frame(
      plant_id = rep(paste0("P", seq_len(n)), 2),
      stage = factor(rep(c("Pre-Stress", "Post-Stress"), each = n),
                     levels = c("Pre-Stress", "Post-Stress")),
      chlorophyll = round(c(base_y, base_y - rnorm(n, 2.8, 0.9)), 3)
    )
  } else if (design == "repeated") {
    n <- 15
    s_eff <- rnorm(n, 0, 1.8)
    lvls <- c("Week 1", "Week 2", "Week 3", "Week 4")
    data.frame(
      plant_id = factor(rep(paste0("P", seq_len(n)), 4)),
      timepoint = factor(rep(lvls, each = n), levels = lvls),
      shoot_cm = round(c(
        8 + s_eff + rnorm(n, 0, 0.7),
        11 + s_eff + rnorm(n, 0, 0.7),
        15 + s_eff + rnorm(n, 0, 0.8),
        18 + s_eff + rnorm(n, 0, 0.9)
      ), 3)
    )
  } else if (design == "count") {
    n <- 20
    lvls <- c("Control", "+4\u00b0C - 1W", "0\u00b0C - 2W", "-20\u00b0C - Lethal")
    data.frame(
      treatment = factor(rep(lvls, each = n), levels = lvls),
      leaf_count = c(
        stats::rnbinom(n, mu = 11, size = 2.0),
        stats::rnbinom(n, mu = 6, size = 2.0),
        stats::rnbinom(n, mu = 3, size = 2.0),
        rep(0L, n)
      )
    )
  } else if (design == "ancova") {
    n <- 20
    init_mm <- round(rnorm(3 * n, mean = 4.5, sd = 1.0), 3)
    lvls <- c("Control", "Primed - Low", "Primed - High")
    grp <- factor(rep(lvls, each = n), levels = lvls)
    final_cm <- round(5 + 1.6 * init_mm + rep(c(0, 2.8, 5.5), each = n) + rnorm(3 * n, 0, 1.0), 3)
    data.frame(treatment = grp, initial_mm = init_mm, final_cm = final_cm)
  } else {
    lvls <- c("Control", "Bio-Fert A", "Bio-Fert B", "NPK Std")
    blks <- paste0("Block_", 1:6)
    g <- expand.grid(treatment = lvls, block = blks, stringsAsFactors = FALSE)
    g$treatment <- factor(g$treatment, levels = lvls)
    g$block <- factor(g$block, levels = blks)
    b_eff <- stats::setNames(c(-3.0, -1.8, -0.5, 0.8, 2.0, 3.2), blks)
    t_eff <- stats::setNames(c(18, 23, 27, 31), lvls)
    g$yield_kg <- round(t_eff[as.character(g$treatment)] +
                          b_eff[as.character(g$block)] + rnorm(nrow(g), 0, 1.1), 3)
    g
  }
}

designs_v02_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::h4(shiny::textOutput(ns("hdr_sidebar"), inline = TRUE)),
      shiny::uiOutput(ns("design_selector_ui")),
      shiny::uiOutput(ns("demo_chk_ui")),
      shiny::hr(),
      shiny::uiOutput(ns("dynamic_inputs")),
      shiny::hr(),
      shiny::numericInput(ns("alpha"), label = "Significance Level (\u03b1):",
                          value = 0.05, min = 0.001, max = 0.2, step = 0.01),
      shiny::radioButtons(ns("plot_style"), label = "Plot Style:",
                          choices = c("Point + CI/SD" = "pointrange", "Bar + CI/SD" = "bar"),
                          inline = TRUE),
      shiny::actionButton(ns("run_btn"), label = shiny::textOutput(ns("lbl_run"), inline = TRUE),
                          icon = shiny::icon("play"), class = "btn-success btn-block")
    ),
    shiny::mainPanel(
      width = 9,
      shiny::uiOutput(ns("error_banner")),
      shiny::tabsetPanel(
        shiny::tabPanel(
          title = shiny::textOutput(ns("lbl_tab_sum"), inline = TRUE),
          shiny::br(),
          shiny::verbatimTextOutput(ns("summary_print")),
          shiny::br(),
          DT::DTOutput(ns("desc_table"))
        ),
        shiny::tabPanel(
          title = shiny::textOutput(ns("lbl_tab_pw"), inline = TRUE),
          shiny::br(),
          shiny::uiOutput(ns("pw_note")),
          DT::DTOutput(ns("pw_table"))
        ),
        shiny::tabPanel(
          title = shiny::textOutput(ns("lbl_tab_plt"), inline = TRUE),
          shiny::br(),
          shiny::plotOutput(ns("cld_plot"), height = "480px"),
          shiny::br(),
          shiny::downloadButton(ns("dl_plot"), label = shiny::textOutput(ns("lbl_dl_png"), inline = TRUE),
                                class = "btn-primary")
        ),
        shiny::tabPanel(
          title = shiny::textOutput(ns("lbl_tab_rep"), inline = TRUE),
          shiny::br(),
          shiny::h5("Methods Paragraph"),
          shiny::verbatimTextOutput(ns("methods_txt")),
          shiny::hr(),
          shiny::h5("Reproducible R Script"),
          shiny::verbatimTextOutput(ns("r_script_txt")),
          shiny::downloadButton(ns("dl_script"), label = shiny::textOutput(ns("lbl_dl_r"), inline = TRUE),
                                class = "btn-primary")
        )
      )
    )
  )
}

designs_v02_server <- function(id, data_r, lang_r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tr2 <- function(key) {
      lng <- if (is.null(lang_r())) "en" else lang_r()
      .v02_dict[[key]][[lng]]
    }

    output$hdr_sidebar <- shiny::renderText(tr2("sidebar_hdr"))
    output$lbl_run     <- shiny::renderText(tr2("run_btn"))
    output$lbl_tab_sum <- shiny::renderText(tr2("tab_sum"))
    output$lbl_tab_pw  <- shiny::renderText(tr2("tab_pw"))
    output$lbl_tab_plt <- shiny::renderText(tr2("tab_plt"))
    output$lbl_tab_rep <- shiny::renderText(tr2("tab_rep"))
    output$lbl_dl_png  <- shiny::renderText(tr2("dl_png"))
    output$lbl_dl_r    <- shiny::renderText(tr2("dl_r"))

    output$design_selector_ui <- shiny::renderUI({
      lng <- if (is.null(lang_r())) "en" else lang_r()
      ch <- if (lng == "tr") {
        c(
          "E\u015fle\u015ftirilmi\u015f 2 Ko\u015ful (Paired)" = "paired",
          "Tekrarl\u0131 \u00d6l\u00e7\u00fcmler (k >= 3)" = "repeated",
          "Say\u0131m Verisi (Poisson / Negatif Binom)" = "count",
          "Kovaryans Analizi (ANCOVA)" = "ancova",
          "Tesad\u00fcf Bloklar\u0131 (RCBD)" = "rcbd"
        )
      } else {
        c(
          "Paired 2-Condition (t / Wilcoxon)" = "paired",
          "Repeated Measures (k >= 3)" = "repeated",
          "Count GLM (Poisson / Neg. Binomial)" = "count",
          "ANCOVA (Covariate-Adjusted)" = "ancova",
          "Block Design (RCBD)" = "rcbd"
        )
      }
      sel <- shiny::isolate(input$design)
      if (is.null(sel) || !sel %in% ch) sel <- "rcbd"
      shiny::selectInput(ns("design"), label = tr2("design_lbl"), choices = ch, selected = sel)
    })

    output$demo_chk_ui <- shiny::renderUI({
      val <- shiny::isolate(input$use_demo)
      if (is.null(val)) val <- TRUE
      shiny::checkboxInput(ns("use_demo"), label = tr2("use_demo"), value = val)
    })

    shiny::observeEvent(lang_r(), {
      shiny::updateNumericInput(session, "alpha", label = tr2("alpha_lbl"))
      shiny::updateRadioButtons(session, "plot_style", label = tr2("style_lbl"))
    }, ignoreInit = TRUE)

    active_df <- shiny::reactive({
      des <- if (is.null(input$design)) "rcbd" else input$design
      if (isTRUE(input$use_demo)) {
        .v02_demo_data(des)
      } else {
        d <- data_r()
        shiny::req(is.data.frame(d), nrow(d) > 0)
        d
      }
    })

    output$dynamic_inputs <- shiny::renderUI({
      df <- active_df()
      des <- if (is.null(input$design)) "rcbd" else input$design
      cols <- names(df)
      num_cols <- cols[vapply(df, is.numeric, logical(1))]
      cat_cols <- cols[!vapply(df, is.numeric, logical(1))]
      if (length(cat_cols) == 0L) cat_cols <- cols
      if (length(num_cols) == 0L) num_cols <- cols

      if (des %in% c("paired", "repeated")) {
        shiny::tagList(
          shiny::selectInput(ns("resp"), tr2("resp_lbl"), choices = num_cols,
                             selected = tail(num_cols, 1)),
          shiny::selectInput(ns("cond"), tr2("cond_lbl"), choices = cols,
                             selected = if (length(cat_cols) >= 2L) cat_cols[2L] else cat_cols[1L]),
          shiny::selectInput(ns("subj_id"), tr2("id_lbl"), choices = cols,
                             selected = cat_cols[1L]),
          shiny::selectInput(ns("force_method"), tr2("mode_lbl"),
                             choices = c("Auto" = "auto", "Parametric" = "parametric",
                                         "Non-parametric" = "nonparametric")),
          if (des == "repeated") {
            shiny::selectInput(ns("p_adj"), tr2("padj_lbl"),
                               choices = c("Holm" = "holm", "Bonferroni" = "bonferroni", "BH" = "BH"))
          }
        )
      } else if (des == "count") {
        shiny::tagList(
          shiny::selectInput(ns("resp"), tr2("resp_lbl"), choices = num_cols,
                             selected = tail(num_cols, 1)),
          shiny::selectInput(ns("grp"), tr2("grp_lbl"), choices = cols,
                             selected = cat_cols[1L]),
          shiny::selectInput(ns("offset_col"), tr2("off_lbl"),
                             choices = c("None" = "", num_cols), selected = ""),
          shiny::selectInput(ns("force_family"), tr2("fam_lbl"),
                             choices = c("Auto (Poisson -> NegBin)" = "auto",
                                         "Poisson" = "poisson",
                                         "Negative Binomial" = "negbin",
                                         "Quasi-Poisson" = "quasipoisson")),
          shiny::selectInput(ns("p_adj"), tr2("padj_lbl"),
                             choices = c("Holm" = "holm", "Bonferroni" = "bonferroni", "BH" = "BH"))
        )
      } else if (des == "ancova") {
        shiny::tagList(
          shiny::selectInput(ns("resp"), tr2("resp_lbl"), choices = num_cols,
                             selected = tail(num_cols, 1)),
          shiny::selectInput(ns("grp"), tr2("grp_lbl"), choices = cols,
                             selected = cat_cols[1L]),
          shiny::selectInput(ns("cov_col"), tr2("cov_lbl"), choices = num_cols,
                             selected = num_cols[1L]),
          shiny::checkboxInput(ns("chk_homog"), tr2("homog_lbl"), value = TRUE),
          shiny::selectInput(ns("p_adj"), tr2("padj_lbl"),
                             choices = c("Holm" = "holm", "Bonferroni" = "bonferroni", "BH" = "BH"))
        )
      } else {
        shiny::tagList(
          shiny::selectInput(ns("resp"), tr2("resp_lbl"), choices = num_cols,
                             selected = tail(num_cols, 1)),
          shiny::selectInput(ns("grp"), tr2("grp_lbl"), choices = cols,
                             selected = cat_cols[1L]),
          shiny::selectInput(ns("blk_col"), tr2("blk_lbl"), choices = cols,
                             selected = if (length(cat_cols) >= 2L) cat_cols[2L] else cat_cols[1L]),
          shiny::selectInput(ns("blk_type"), tr2("blk_type"),
                             choices = c("Fixed Block (ANOVA)" = "fixed",
                                         "Random Block (lme4 Mixed)" = "random")),
          shiny::selectInput(ns("force_method"), tr2("mode_lbl"),
                             choices = c("Auto" = "auto", "Parametric" = "parametric",
                                         "Non-parametric (Friedman)" = "nonparametric")),
          shiny::selectInput(ns("p_adj"), tr2("padj_lbl"),
                             choices = c("Tukey-Kramer" = "tukey", "Holm" = "holm",
                                         "Bonferroni" = "bonferroni"))
        )
      }
    })

    res_rv <- shiny::reactiveVal(NULL)
    err_rv <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run_btn, {
      err_rv(NULL)
      df <- active_df()
      des <- input$design
      alp <- if (is.null(input$alpha) || is.na(input$alpha)) 0.05 else input$alpha

      out <- tryCatch({
        if (des == "paired") {
          statarbor::compare_paired(
            data = df, response = input$resp, condition = input$cond,
            id = input$subj_id, alpha = alp, force_method = input$force_method
          )
        } else if (des == "repeated") {
          statarbor::auto_repeated(
            data = df, response = input$resp, condition = input$cond,
            id = input$subj_id, alpha = alp, p_adjust = input$p_adj,
            force_method = input$force_method
          )
        } else if (des == "count") {
          off_val <- if (is.null(input$offset_col) || !nzchar(input$offset_col)) NULL else input$offset_col
          statarbor::analyze_count(
            data = df, response = input$resp, group = input$grp,
            offset = off_val, alpha = alp, p_adjust = input$p_adj,
            force_family = input$force_family
          )
        } else if (des == "ancova") {
          statarbor::analyze_ancova(
            data = df, response = input$resp, group = input$grp,
            covariate = input$cov_col, alpha = alp, p_adjust = input$p_adj,
            check_homogeneity = isTRUE(input$chk_homog)
          )
        } else {
          statarbor::analyze_rcbd(
            data = df, response = input$resp, treatment = input$grp,
            block = input$blk_col, block_type = input$blk_type,
            alpha = alp, p_adjust = input$p_adj, force_method = input$force_method
          )
        }
      }, error = function(e) {
        err_rv(conditionMessage(e))
        NULL
      })
      res_rv(out)
    })

    output$error_banner <- shiny::renderUI({
      msg <- err_rv()
      if (is.null(msg)) return(NULL)
      shiny::div(class = "alert alert-danger", shiny::strong("Error: "), msg)
    })

    output$summary_print <- shiny::renderPrint({
      res <- res_rv()
      if (is.null(res)) {
        cat(tr2("run_hint"), "\n")
      } else {
        print(res)
      }
    })

    output$desc_table <- DT::renderDT({
      res <- res_rv()
      shiny::req(res)
      d <- res$descriptives
      num_i <- vapply(d, is.numeric, logical(1))
      d[num_i] <- lapply(d[num_i], function(v) round(v, 4))
      DT::datatable(d, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
    })

    output$pw_note <- shiny::renderUI({
      res <- res_rv()
      if (!is.null(res) && inherits(res, "statarbor_paired")) {
        shiny::div(class = "alert alert-info", tr2("no_pw"))
      } else {
        NULL
      }
    })

    output$pw_table <- DT::renderDT({
      res <- res_rv()
      shiny::req(res, !is.null(res$pairwise))
      d <- res$pairwise
      num_i <- vapply(d, is.numeric, logical(1))
      d[num_i] <- lapply(d[num_i], function(v) round(v, 4))
      DT::datatable(d, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
    })

    output$cld_plot <- shiny::renderPlot({
      res <- res_rv()
      shiny::req(res)
      plot(res, style = input$plot_style)
    })

    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0("statarbor_", input$design, "_plot.png"),
      content = function(file) {
        res <- res_rv()
        shiny::req(res)
        p <- plot(res, style = input$plot_style)
        ggplot2::ggsave(file, plot = p, width = 8, height = 5.5, dpi = 300)
      }
    )

    output$methods_txt <- shiny::renderText({
      res <- res_rv()
      if (is.null(res)) return(tr2("run_hint"))
      lng <- if (is.null(lang_r())) "en" else lang_r()
      statarbor::generate_methods_text(res, lang = lng)
    })

    output$r_script_txt <- shiny::renderText({
      res <- res_rv()
      if (is.null(res)) return("")
      paste(statarbor::export_r_script(res, data_name = "my_data"), collapse = "\n")
    })

    output$dl_script <- shiny::downloadHandler(
      filename = function() paste0("statarbor_", input$design, "_reproducible.R"),
      content = function(file) {
        res <- res_rv()
        shiny::req(res)
        statarbor::export_r_script(res, data_name = "my_data", file = file)
      }
    )
  })
}
