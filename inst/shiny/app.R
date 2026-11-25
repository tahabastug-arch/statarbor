################################################################################
# statarbor: Bilingual (EN / TR) Interactive Statistical Decision Application
################################################################################

library(shiny)
library(DT)
library(ggplot2)
library(statarbor)

# ==============================================================================
# 1. BILINGUAL DICTIONARY (ASCII-SAFE UNICODE ESCAPES)
# ==============================================================================

i18n_dict <- list(
  subtitle = c(
    en = "Assumption-Guided Statistical Decision Engine & Reporting",
    tr = "Varsay\u0131m Denetimli \u0130statistiksel Karar Motoru ve Raporlama"
  ),
  tab1 = c(en = "1. Data", tr = "1. Veri"),
  tab2 = c(en = "2. Auto-Compare", tr = "2. Otomatik Analiz"),
  tab3 = c(en = "3. CLD Plots", tr = "3. Harfli Grafikler"),
  tab4 = c(en = "4. Advanced Models", tr = "4. \u0130leri Modeller"),
  tab5 = c(en = "5. Multivariate", tr = "5. \u00c7ok De\u011fi\u015fkenli"),
  tab6 = c(en = "About & Cite", tr = "Hakk\u0131nda & At\u0131f"),

  # Tab 1
  data_src     = c(en = "Data Source", tr = "Veri Kayna\u011f\u0131"),
  upload_lbl   = c(en = "Upload CSV / TXT File", tr = "CSV / TXT Dosyas\u0131 Y\u00fckle"),
  upload_help  = c(en = "Comma (,), semicolon (;), or tab delimiters are auto-detected.",
                   tr = "Virg\u00fcl (,), noktal\u0131 virg\u00fcl (;) veya sekme (Tab) otomatik tan\u0131n\u0131r."),
  bench_hdr    = c(en = "Built-in Benchmark Dataset", tr = "Yerle\u015fik \u00d6rnek Veri Seti"),
  load_bench   = c(en = "Load thermal_stress (n=120)", tr = "thermal_stress Y\u00fckle (n=120)"),
  welcome_txt  = c(en = "Upload your own experimental dataset or load the built-in thermal_stress benchmark dataset. Use the Data Preparation panel below to merge numbered replicate labels (e.g., 'Control 1..63' -> 'Control') or combine two factors into a single grouping variable.",
                   tr = "Kendi deneysel veri setinizi y\u00fckleyin veya yerle\u015fik thermal_stress verisini test edin. Numaraland\u0131r\u0131lm\u0131\u015f etiketleri ('Kontrol 1..63' -> 'Kontrol') birle\u015ftirmek veya iki fakt\u00f6r\u00fc birle\u015ftirmek i\u00e7in a\u015fa\u011f\u0131daki Veri Haz\u0131rlama arac\u0131n\u0131 kullanabilirsiniz."),
  prep_hdr     = c(en = "Data Preparation (Optional)", tr = "Veri Haz\u0131rlama (\u0130ste\u011fe Ba\u011fl\u0131)"),
  prep_pref_h  = c(en = "Merge Numbered Group Prefix", tr = "Numaral\u0131 Grup \u00d6neklerini Birle\u015ftir"),
  prep_comb_h  = c(en = "Combine Two Factors", tr = "\u0130ki Fakt\u00f6r\u00fc Birle\u015ftir"),
  btn_merge    = c(en = "Merge Prefix", tr = "\u00d6neki Birle\u015ftir"),
  btn_combine  = c(en = "Combine Factors", tr = "Fakt\u00f6rleri Birle\u015ftir"),
  btn_reset    = c(en = "Reset to Raw Data", tr = "Ham Veriye S\u0131f\u0131rla"),
  preview_hdr  = c(en = "Dataset Preview", tr = "Veri \u00d6nizlemesi"),

  # Tab 2
  var_sel      = c(en = "Variable Selection", tr = "De\u011fi\u015fken Se\u00e7imi"),
  grp_col      = c(en = "Grouping Factor:", tr = "Gruplama Fakt\u00f6r\u00fc:"),
  resp_cols    = c(en = "Numeric Response Variable(s):", tr = "Say\u0131sal Ba\u011f\u0131ml\u0131 De\u011fi\u015fken(ler):"),
  strategy     = c(en = "Decision Strategy:", tr = "Analiz Stratejisi:"),
  dunn_adj     = c(en = "Dunn P-Adjustment:", tr = "Dunn p-D\u00fczeltmesi:"),
  alpha_lbl    = c(en = "Significance Level (\u03b1):", tr = "Anlaml\u0131l\u0131k D\u00fczeyi (\u03b1):"),
  run_btn      = c(en = "Run Analysis", tr = "Analizi \u00c7al\u0131\u015ft\u0131r"),
  sub_summary  = c(en = "Decision Summary", tr = "Karar \u00d6zeti"),
  sub_desc     = c(en = "Descriptives & CLD", tr = "Betimsel & Harf Gruplar\u0131"),
  sub_posthoc  = c(en = "Post-Hoc Pairwise", tr = "\u0130kili Post-Hoc"),
  sub_repro    = c(en = "Methods Text & R Code", tr = "Y\u00f6ntem Metni & R Kodu"),
  detail_var   = c(en = "Inspect Variable:", tr = "Detay\u0131 G\u00f6sterilecek De\u011fi\u015fken:"),

  # Tab 3
  plot_cfg     = c(en = "Figure Settings", tr = "Grafik Ayarlar\u0131"),
  plot_var     = c(en = "Variable to Plot:", tr = "Grafi\u011fi \u00c7izilecek De\u011fi\u015fken:"),
  plot_type    = c(en = "Geometry:", tr = "Grafik T\u00fcr\u00fc:"),
  show_cld     = c(en = "Show CLD Letters", tr = "Harf Gruplar\u0131n\u0131 G\u00f6ster"),
  font_lbl     = c(en = "Font Family:", tr = "Yaz\u0131 Tipi:"),
  size_lbl     = c(en = "Font Size (pt):", tr = "Yaz\u0131 Boyutu (pt):"),
  angle_lbl    = c(en = "X-Axis Label Angle:", tr = "X Ekseni Yaz\u0131 A\u00e7\u0131s\u0131:"),
  dpi_lbl      = c(en = "Export Resolution:", tr = "\u0130ndirme \u00c7\u00f6z\u00fcn\u00fcrl\u00fc\u011f\u00fc:"),

  # Tab 4
  adv_2g       = c(en = "Two-Group (t-test / Mann-Whitney)", tr = "\u0130ki Grup (t-testi / Mann-Whitney)"),
  adv_bin      = c(en = "Binary & Firth Logistic", tr = "\u0130kili & Firth Lojistik"),
  adv_ord      = c(en = "Ordinal Regression (CLM)", tr = "Ordinal Regresyon (CLM)"),
  adv_glm      = c(en = "Factorial Interaction GLM", tr = "Fakt\u00f6riyel Etkile\u015fim GLM"),

  # Tab 5
  multi_cfg    = c(en = "Multivariate Settings", tr = "\u00c7ok De\u011fi\u015fkenli Ayarlar"),
  multi_vars   = c(en = "Numeric Variables (>= 2):", tr = "Say\u0131sal De\u011fi\u015fkenler (En az 2):"),
  cor_method   = c(en = "Correlation Method:", tr = "Korelasyon Y\u00f6ntemi:"),
  pca_grp      = c(en = "PCA Color Group:", tr = "PCA Renklendirme Grubu:"),
  pca_ell      = c(en = "Show Confidence Ellipses", tr = "G\u00fcven Elipslerini G\u00f6ster")
)

load_benchmark_data <- function() {
  e <- new.env(parent = emptyenv())
  utils::data("thermal_stress", package = "statarbor", envir = e)
  e$thermal_stress
}

# ==============================================================================
# 2. USER INTERFACE (UI)
# ==============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #F8FAFC; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }
      .sa-header {
        background: linear-gradient(135deg, #1B365D 0%, #2B4C7E 100%);
        color: #FFFFFF; padding: 14px 22px; margin: 0 -15px 15px -15px;
        display: flex; justify-content: space-between; align-items: center;
        box-shadow: 0 2px 6px rgba(0,0,0,0.12);
      }
      .sa-brand { font-size: 22px; font-weight: 700; letter-spacing: 0.4px; }
      .sa-sub { font-size: 13px; color: #CBD5E1; margin-top: 2px; }
      .sa-lang .radio-inline { color: #FFFFFF; font-weight: 600; margin-left: 10px; }
      .sa-info { background: #EFF6FF; border-left: 4px solid #1B365D; padding: 12px 15px; border-radius: 4px; margin-bottom: 15px; }
      .sa-box { background: #FFFFFF; border: 1px solid #E2E8F0; border-radius: 6px; padding: 15px; margin-bottom: 15px; }
      .sa-subbox { background: #F8FAFC; border: 1px solid #E2E8F0; border-radius: 6px; padding: 12px; height: 100%; }
      .well { background-color: #FFFFFF; border: 1px solid #E2E8F0; border-radius: 6px; }
      pre { background-color: #0F172A; color: #E2E8F0; border-radius: 6px; padding: 14px; font-size: 12.5px; }
    "))
  ),

  # Top Header with Language Switch
  div(
    class = "sa-header",
    div(
      div(class = "sa-brand", "statarbor"),
      div(class = "sa-sub", textOutput("txt_subtitle", inline = TRUE))
    ),
    div(
      class = "sa-lang",
      radioButtons(
        "lang", label = NULL,
        choices = c("English" = "en", "T\u00fcrk\u00e7e" = "tr"),
        selected = "en", inline = TRUE
      )
    )
  ),

  tabsetPanel(
    id = "main_tabs",
    type = "pills",

    # --- TAB 1: DATA & PREPARATION ---
    tabPanel(
      title = textOutput("txt_tab1", inline = TRUE),
      value = "tab_data",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4(textOutput("txt_data_src", inline = TRUE)),
          fileInput("file_upload", label = textOutput("txt_upload_lbl", inline = TRUE),
                    accept = c(".csv", ".txt")),
          helpText(textOutput("txt_upload_help", inline = TRUE)),
          hr(),
          h4(textOutput("txt_bench_hdr", inline = TRUE)),
          actionButton("load_bench", label = textOutput("txt_load_bench", inline = TRUE),
                       class = "btn-primary btn-block", icon = icon("database")),
          br(),
          downloadButton("dl_bench", label = "CSV", class = "btn-default btn-block"),
          hr(),
          uiOutput("data_summary_ui")
        ),
        mainPanel(
          width = 9,
          div(class = "sa-info", textOutput("txt_welcome")),
          uiOutput("ui_data_prep_panel"),
          div(class = "sa-box",
              h4(textOutput("txt_preview_hdr", inline = TRUE)),
              DTOutput("data_preview"))
        )
      )
    ),

    # --- TAB 2: AUTO-COMPARE ---
    tabPanel(
      title = textOutput("txt_tab2", inline = TRUE),
      value = "tab_auto",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4(textOutput("txt_var_sel", inline = TRUE)),
          uiOutput("ui_group_var"),
          uiOutput("ui_response_vars"),
          hr(),
          selectInput("analysis_mode", label = "Decision Strategy:",
                      choices = c("Auto Decision Tree" = "auto",
                                  "One-way ANOVA + Tukey HSD" = "anova",
                                  "Welch ANOVA + Games-Howell" = "welch",
                                  "Kruskal-Wallis + Dunn" = "kruskal")),
          selectInput("p_adjust", label = "Dunn P-Adjustment:",
                      choices = c("Bonferroni" = "bonferroni",
                                  "Holm" = "holm",
                                  "Benjamini-Hochberg (FDR)" = "BH")),
          numericInput("alpha_level", label = "Alpha (\u03b1):",
                       value = 0.05, min = 0.001, max = 0.1, step = 0.01),
          actionButton("run_auto", label = textOutput("txt_run_btn", inline = TRUE),
                       class = "btn-success btn-block", icon = icon("play")),
          hr(),
          downloadButton("dl_docx", label = "Word (.docx)", class = "btn-primary btn-block")
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel(
              title = textOutput("txt_sub_summary", inline = TRUE),
              br(),
              div(class = "sa-box", DTOutput("tbl_summary"))
            ),
            tabPanel(
              title = textOutput("txt_sub_desc", inline = TRUE),
              br(),
              div(class = "sa-box",
                  uiOutput("ui_detail_var"),
                  DTOutput("tbl_desc"))
            ),
            tabPanel(
              title = textOutput("txt_sub_posthoc", inline = TRUE),
              br(),
              div(class = "sa-box", DTOutput("tbl_posthoc"))
            ),
            tabPanel(
              title = textOutput("txt_sub_repro", inline = TRUE),
              br(),
              div(class = "sa-box",
                  h4("Publication-Ready Methods Paragraph"),
                  div(class = "sa-info", textOutput("txt_methods_para")),
                  hr(),
                  h4("Reproducible R Script"),
                  downloadButton("dl_rscript", label = "Download .R", class = "btn-primary"),
                  br(), br(),
                  verbatimTextOutput("txt_rscript"))
            )
          )
        )
      )
    ),

    # --- TAB 3: CLD PLOTS ---
    tabPanel(
      title = textOutput("txt_tab3", inline = TRUE),
      value = "tab_plots",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4(textOutput("txt_plot_cfg", inline = TRUE)),
          uiOutput("ui_plot_var"),
          radioButtons("plot_type", label = "Geometry:",
                       choices = c("Boxplot" = "boxplot", "Violin + Boxplot" = "violin")),
          checkboxInput("show_letters", label = "CLD Letters", value = TRUE),
          selectInput("font_family", label = "Font:",
                      choices = c("Serif (Times)" = "serif", "Sans-Serif (Arial)" = "sans")),
          sliderInput("font_size", label = "Font Size (pt):", min = 9, max = 18, value = 12),
          sliderInput("x_angle", label = "X-Axis Angle:", min = 0, max = 90, value = 45, step = 15),
          selectInput("plot_dpi", label = "PNG DPI:", choices = c("300 DPI" = 300, "600 DPI" = 600)),
          downloadButton("dl_plot", label = "PNG", class = "btn-primary btn-block")
        ),
        mainPanel(
          width = 9,
          div(class = "sa-box", plotOutput("cld_plot", height = "580px"))
        )
      )
    ),

    # --- TAB 4: ADVANCED MODELS ---
    tabPanel(
      title = textOutput("txt_tab4", inline = TRUE),
      value = "tab_adv",
      br(),
      tabsetPanel(
        tabPanel(
          title = textOutput("txt_adv_2g", inline = TRUE),
          br(),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              uiOutput("ui_2g_group"),
              uiOutput("ui_2g_resp"),
              selectInput("mode_2g", "Method:",
                          choices = c("Auto" = "auto",
                                      "Welch t-test" = "t.test",
                                      "Mann-Whitney U" = "mann_whitney"))
            ),
            mainPanel(
              width = 9,
              div(class = "sa-box",
                  fluidRow(
                    column(6, plotOutput("plot_2g", height = "400px")),
                    column(6, verbatimTextOutput("out_2g"))
                  ))
            )
          )
        ),
        tabPanel(
          title = textOutput("txt_adv_bin", inline = TRUE),
          br(),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              uiOutput("ui_bin_resp"),
              uiOutput("ui_bin_group"),
              selectInput("firth_mode", "Firth Correction:",
                          choices = c("Auto (On Complete Separation)" = "auto",
                                      "Always" = "always",
                                      "Never (Standard GLM)" = "never"))
            ),
            mainPanel(
              width = 9,
              div(class = "sa-box",
                  verbatimTextOutput("out_bin_summary"),
                  h4("Odds Ratios & Confidence Intervals"),
                  DTOutput("tbl_bin_coef"))
            )
          )
        ),
        tabPanel(
          title = textOutput("txt_adv_ord", inline = TRUE),
          br(),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              uiOutput("ui_ord_resp"),
              uiOutput("ui_ord_group")
            ),
            mainPanel(
              width = 9,
              div(class = "sa-box",
                  verbatimTextOutput("out_ord_summary"),
                  h4("Cumulative Link Model Coefficients"),
                  DTOutput("tbl_ord_coef"))
            )
          )
        ),
        tabPanel(
          title = textOutput("txt_adv_glm", inline = TRUE),
          br(),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              uiOutput("ui_glm_resp"),
              uiOutput("ui_glm_factors"),
              selectInput("glm_family", "Model Family:",
                          choices = c("Gaussian (Identity)" = "gaussian",
                                      "Gamma (Log link, > 0)" = "gamma_log"))
            ),
            mainPanel(
              width = 9,
              div(class = "sa-box",
                  verbatimTextOutput("out_glm_summary"),
                  h4("Deviance ANOVA Table"),
                  DTOutput("tbl_glm_anova"))
            )
          )
        )
      )
    ),

    # --- TAB 5: MULTIVARIATE ---
    tabPanel(
      title = textOutput("txt_tab5", inline = TRUE),
      value = "tab_multi",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4(textOutput("txt_multi_cfg", inline = TRUE)),
          uiOutput("ui_multi_vars"),
          selectInput("cor_method", label = "Correlation:",
                      choices = c("Pearson r" = "pearson", "Spearman rho" = "spearman")),
          uiOutput("ui_pca_group"),
          checkboxInput("pca_ellipse", label = "Ellipses", value = FALSE),
          downloadButton("dl_cor", label = "Correlation PNG", class = "btn-default btn-block"),
          br(),
          downloadButton("dl_pca", label = "PCA Biplot PNG", class = "btn-default btn-block")
        ),
        mainPanel(
          width = 9,
          fluidRow(
            column(6, div(class = "sa-box", plotOutput("plot_cor", height = "480px"))),
            column(6, div(class = "sa-box", plotOutput("plot_pca", height = "480px")))
          )
        )
      )
    ),

    # --- TAB 6: ABOUT & CITE ---
    tabPanel(
      title = textOutput("txt_tab6", inline = TRUE),
      value = "tab_about",
      br(),
      div(class = "sa-box", style = "max-width: 900px; margin: 0 auto;",
          uiOutput("about_content"))
    )
  )
)

# ==============================================================================
# 3. SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {

  tr <- function(key) {
    lng <- if (is.null(input$lang)) "en" else input$lang
    i18n_dict[[key]][[lng]]
  }

  # Dynamic UI text outputs
  output$txt_subtitle    <- renderText(tr("subtitle"))
  output$txt_tab1        <- renderText(tr("tab1"))
  output$txt_tab2        <- renderText(tr("tab2"))
  output$txt_tab3        <- renderText(tr("tab3"))
  output$txt_tab4        <- renderText(tr("tab4"))
  output$txt_tab5        <- renderText(tr("tab5"))
  output$txt_tab6        <- renderText(tr("tab6"))
  output$txt_data_src    <- renderText(tr("data_src"))
  output$txt_upload_lbl  <- renderText(tr("upload_lbl"))
  output$txt_upload_help <- renderText(tr("upload_help"))
  output$txt_bench_hdr   <- renderText(tr("bench_hdr"))
  output$txt_load_bench  <- renderText(tr("load_bench"))
  output$txt_welcome     <- renderText(tr("welcome_txt"))
  output$txt_preview_hdr <- renderText(tr("preview_hdr"))
  output$txt_var_sel     <- renderText(tr("var_sel"))
  output$txt_run_btn     <- renderText(tr("run_btn"))
  output$txt_sub_summary <- renderText(tr("sub_summary"))
  output$txt_sub_desc    <- renderText(tr("sub_desc"))
  output$txt_sub_posthoc <- renderText(tr("sub_posthoc"))
  output$txt_sub_repro   <- renderText(tr("sub_repro"))
  output$txt_plot_cfg    <- renderText(tr("plot_cfg"))
  output$txt_adv_2g      <- renderText(tr("adv_2g"))
  output$txt_adv_bin     <- renderText(tr("adv_bin"))
  output$txt_adv_ord     <- renderText(tr("adv_ord"))
  output$txt_adv_glm     <- renderText(tr("adv_glm"))
  output$txt_multi_cfg   <- renderText(tr("multi_cfg"))

  observeEvent(input$lang, {
    updateSelectInput(session, "analysis_mode", label = tr("strategy"))
    updateSelectInput(session, "p_adjust", label = tr("dunn_adj"))
    updateNumericInput(session, "alpha_level", label = tr("alpha_lbl"))
    updateRadioButtons(session, "plot_type", label = tr("plot_type"))
    updateCheckboxInput(session, "show_letters", label = tr("show_cld"))
    updateSelectInput(session, "font_family", label = tr("font_lbl"))
    updateSliderInput(session, "font_size", label = tr("size_lbl"))
    updateSliderInput(session, "x_angle", label = tr("angle_lbl"))
    updateSelectInput(session, "plot_dpi", label = tr("dpi_lbl"))
    updateSelectInput(session, "cor_method", label = tr("cor_method"))
    updateCheckboxInput(session, "pca_ellipse", label = tr("pca_ell"))
  })

  # Reactive state
  rv <- reactiveValues(
    raw_data = NULL,
    data = NULL,
    preferred_group = NULL,
    results = list(),
    errors = list()
  )

  # Load benchmark dataset
  observeEvent(input$load_bench, {
    df <- load_benchmark_data()
    rv$raw_data <- df
    rv$data <- df
    rv$preferred_group <- "treatment"
    rv$results <- list()
    rv$errors <- list()
    showNotification("thermal_stress (120 x 10) loaded.", type = "message")
  })

  # Upload user CSV / TXT with auto delimiter detection
  observeEvent(input$file_upload, {
    req(input$file_upload)
    path <- input$file_upload$datapath
    tryCatch({
      first_line <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
      if (grepl(";", first_line, fixed = TRUE)) {
        df <- utils::read.delim(path, sep = ";", stringsAsFactors = FALSE,
                                fileEncoding = "UTF-8", check.names = FALSE)
      } else if (grepl("\t", first_line, fixed = TRUE)) {
        df <- utils::read.delim(path, sep = "\t", stringsAsFactors = FALSE,
                                fileEncoding = "UTF-8", check.names = FALSE)
      } else {
        df <- utils::read.csv(path, stringsAsFactors = FALSE,
                              fileEncoding = "UTF-8", check.names = FALSE)
      }
      rv$raw_data <- df
      rv$data <- df
      rv$preferred_group <- NULL
      rv$results <- list()
      rv$errors <- list()
      showNotification(sprintf("Loaded %d rows x %d columns.", nrow(df), ncol(df)),
                       type = "message")
    }, error = function(e) {
      showNotification(paste("File error:", e$message), type = "error")
    })
  })

  # --- TAB 1: DATA PREPARATION PANEL ---
  output$ui_data_prep_panel <- renderUI({
    req(rv$data)
    all_c <- names(rv$data)
    is_tr <- identical(input$lang, "tr")

    div(
      class = "sa-box",
      div(
        style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:10px;",
        h4(tr("prep_hdr"), style = "margin:0;"),
        actionButton("btn_reset_data", label = tr("btn_reset"),
                     class = "btn-default btn-xs", icon = icon("undo"))
      ),
      fluidRow(
        column(
          6,
          div(
            class = "sa-subbox",
            h5(strong(tr("prep_pref_h"))),
            selectInput("prep_prefix_col",
                        label = if (is_tr) "Hedef S\u00fctun:" else "Target Column:",
                        choices = all_c, selected = all_c[1]),
            textInput("prep_prefix_txt",
                      label = if (is_tr) "Birle\u015ftirilecek \u00d6nek (\u00d6rn: Kontrol):" else "Prefix to Merge (e.g., Control):",
                      value = "Kontrol"),
            actionButton("btn_merge_prefix", label = tr("btn_merge"),
                         class = "btn-info btn-sm btn-block", icon = icon("compress"))
          )
        ),
        column(
          6,
          div(
            class = "sa-subbox",
            h5(strong(tr("prep_comb_h"))),
            fluidRow(
              column(6, selectInput("prep_f1", label = if (is_tr) "Fakt\u00f6r 1:" else "Factor 1:",
                                    choices = all_c, selected = all_c[1])),
              column(6, selectInput("prep_f2", label = if (is_tr) "Fakt\u00f6r 2:" else "Factor 2:",
                                    choices = all_c, selected = all_c[min(2, length(all_c))]))
            ),
            textInput("prep_sep",
                      label = if (is_tr) "Ay\u0131r\u0131c\u0131:" else "Separator:",
                      value = " - "),
            actionButton("btn_combine_fac", label = tr("btn_combine"),
                         class = "btn-info btn-sm btn-block", icon = icon("link"))
          )
        )
      )
    )
  })

  observeEvent(input$btn_merge_prefix, {
    req(rv$data, input$prep_prefix_col, nzchar(trimws(input$prep_prefix_txt)))
    tryCatch({
      old_n <- length(unique(stats::na.omit(rv$data[[input$prep_prefix_col]])))
      new_col_nm <- paste0(input$prep_prefix_col, "_clean")
      rv$data <- statarbor::merge_group_prefix(
        data = rv$data,
        column = input$prep_prefix_col,
        prefix = input$prep_prefix_txt,
        new_col = new_col_nm,
        ignore_case = TRUE
      )
      new_n <- nlevels(rv$data[[new_col_nm]])
      rv$preferred_group <- new_col_nm
      rv$results <- list()
      showNotification(
        sprintf("Created '%s' (%d -> %d groups).", new_col_nm, old_n, new_n),
        type = "message"
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  observeEvent(input$btn_combine_fac, {
    req(rv$data, input$prep_f1, input$prep_f2)
    if (identical(input$prep_f1, input$prep_f2)) {
      showNotification("Select two different columns.", type = "warning")
      return()
    }
    tryCatch({
      new_col_nm <- paste0(input$prep_f1, "_x_", input$prep_f2)
      rv$data <- statarbor::combine_factors(
        data = rv$data,
        cols = c(input$prep_f1, input$prep_f2),
        sep = input$prep_sep,
        new_col = new_col_nm
      )
      rv$preferred_group <- new_col_nm
      rv$results <- list()
      showNotification(
        sprintf("Created '%s' (%d combined groups).", new_col_nm, nlevels(rv$data[[new_col_nm]])),
        type = "message"
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  observeEvent(input$btn_reset_data, {
    req(rv$raw_data)
    rv$data <- rv$raw_data
    rv$preferred_group <- NULL
    rv$results <- list()
    rv$errors <- list()
    showNotification("Reset to raw dataset.", type = "message")
  })

  output$dl_bench <- downloadHandler(
    filename = function() "statarbor_thermal_stress.csv",
    content = function(file) utils::write.csv(load_benchmark_data(), file, row.names = FALSE)
  )

  output$data_summary_ui <- renderUI({
    req(rv$data)
    n_num <- sum(vapply(rv$data, is.numeric, logical(1)))
    tagList(
      strong(if (input$lang == "tr") "Veri \u00d6zeti:" else "Dataset Info:"),
      tags$ul(
        tags$li(sprintf("%s: %d", if (input$lang == "tr") "Sat\u0131r" else "Rows", nrow(rv$data))),
        tags$li(sprintf("%s: %d", if (input$lang == "tr") "S\u00fctun" else "Columns", ncol(rv$data))),
        tags$li(sprintf("%s: %d", if (input$lang == "tr") "Say\u0131sal" else "Numeric", n_num))
      )
    )
  })

  output$data_preview <- renderDT({
    req(rv$data)
    DT::datatable(rv$data, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  # Column helpers
  numeric_cols <- reactive({
    req(rv$data)
    names(rv$data)[vapply(rv$data, is.numeric, logical(1))]
  })

  factor_cols <- reactive({
    req(rv$data)
    names(rv$data)[vapply(rv$data, function(x) {
      is.character(x) || is.factor(x) || length(unique(stats::na.omit(x))) <= 25L
    }, logical(1))]
  })

  two_level_cols <- reactive({
    req(rv$data)
    names(rv$data)[vapply(rv$data, function(x) {
      length(unique(stats::na.omit(x))) == 2L
    }, logical(1))]
  })

  # --- TAB 2: AUTO-COMPARE ---
  output$ui_group_var <- renderUI({
    req(factor_cols())
    f_cols <- factor_cols()
    sel <- if (!is.null(rv$preferred_group) && rv$preferred_group %in% f_cols) {
      rv$preferred_group
    } else {
      f_cols[1]
    }
    selectInput("group_var", label = tr("grp_col"), choices = f_cols, selected = sel)
  })

  output$ui_response_vars <- renderUI({
    req(numeric_cols())
    default_sel <- setdiff(numeric_cols(), input$group_var)
    selectizeInput("response_vars", label = tr("resp_cols"),
                   choices = numeric_cols(),
                   selected = utils::head(default_sel, 4),
                   multiple = TRUE)
  })

  observeEvent(input$run_auto, {
    req(rv$data, input$group_var, input$response_vars)
    res_list <- list()
    err_list <- list()
    vars <- input$response_vars

    withProgress(message = "statarbor...", value = 0, {
      for (i in seq_along(vars)) {
        v <- vars[i]
        incProgress(1 / length(vars), detail = v)
        tryCatch({
          res_list[[v]] <- statarbor::auto_compare(
            data = rv$data,
            response = v,
            group = input$group_var,
            mode = input$analysis_mode,
            p_adjust = input$p_adjust,
            alpha = input$alpha_level
          )
        }, error = function(e) {
          err_list[[v]] <<- e$message
        })
      }
    })
    rv$results <- res_list
    rv$errors <- err_list
    showNotification(sprintf("Completed: %d variable(s).", length(res_list)), type = "message")
  })

  summary_table_df <- reactive({
    req(length(rv$results) > 0 || length(rv$errors) > 0)
    rows <- list()
    for (v in names(rv$results)) {
      r <- rv$results[[v]]
      lbl_map <- c(eta_squared = "\u03b7\u00b2", omega_squared = "\u03c9\u00b2", est_omega_squared = "est. \u03c9\u00b2", epsilon_squared = "\u03b5\u00b2", cohens_d = "d", r = "r")
      nms <- names(r$effect_size)
      p_nms <- ifelse(nms %in% names(lbl_map), lbl_map[nms], nms)
      eff_txt <- paste(sprintf("%s = %.3f", p_nms, unlist(r$effect_size)), collapse = ", ")
      rows[[v]] <- data.frame(
        Variable = v,
        Zero_Var = ifelse(r$assumptions$zero_variance, "Yes", "No"),
        Norm_Violated = ifelse(r$assumptions$normality_violated, "Yes", "No"),
        Var_Violated = ifelse(r$assumptions$variance_violated, "Yes", "No"),
        Selected_Method = r$method,
        Statistic = sprintf("%s = %.3f", r$statistic_name, r$statistic),
        p_value = ifelse(r$p_value < 0.001, "< 0.001", sprintf("%.4f", r$p_value)),
        Effect_Size = eff_txt,
        stringsAsFactors = FALSE
      )
    }
    for (v in names(rv$errors)) {
      rows[[v]] <- data.frame(
        Variable = v, Zero_Var = "-", Norm_Violated = "-", Var_Violated = "-",
        Selected_Method = "Error", Statistic = "-", p_value = "-",
        Effect_Size = rv$errors[[v]], stringsAsFactors = FALSE
      )
    }
    do.call(rbind, rows)
  })

  output$tbl_summary <- renderDT({
    req(summary_table_df())
    DT::datatable(
      summary_table_df(),
      extensions = "Buttons",
      options = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel"),
                     pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$ui_detail_var <- renderUI({
    req(length(rv$results) > 0)
    selectInput("detail_var", label = tr("detail_var"), choices = names(rv$results))
  })

  output$tbl_desc <- renderDT({
    req(input$detail_var, rv$results[[input$detail_var]])
    d <- rv$results[[input$detail_var]]$descriptives
    out <- data.frame(
      Group = d$group,
      n = d$n,
      Mean_SD = sprintf("%.3f \u00b1 %s", d$mean, ifelse(is.na(d$sd), "0.000", sprintf("%.3f", d$sd))),
      Median = round(d$median, 3),
      IQR = round(d$iqr, 3),
      Min_Max = sprintf("%.3f - %.3f", d$min, d$max),
      Shapiro_p = ifelse(is.na(d$shapiro_p), "NA", sprintf("%.4f", d$shapiro_p)),
      CLD = d$cld,
      stringsAsFactors = FALSE
    )
    DT::datatable(
      out, extensions = "Buttons",
      options = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel"),
                     pageLength = 20, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$tbl_posthoc <- renderDT({
    req(input$detail_var, rv$results[[input$detail_var]])
    DT::datatable(
      rv$results[[input$detail_var]]$posthoc,
      extensions = "Buttons",
      options = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel"),
                     pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$txt_methods_para <- renderText({
    req(input$detail_var, rv$results[[input$detail_var]])
    statarbor::generate_methods_text(rv$results[[input$detail_var]],
                                     lang = input$lang, include_citation = TRUE)
  })

  output$txt_rscript <- renderText({
    req(input$detail_var, rv$results[[input$detail_var]])
    paste(statarbor::export_r_script(rv$results[[input$detail_var]], data_name = "my_data"),
          collapse = "\n")
  })

  output$dl_rscript <- downloadHandler(
    filename = function() paste0("statarbor_", input$detail_var, "_reproducible.R"),
    content = function(file) {
      req(input$detail_var, rv$results[[input$detail_var]])
      statarbor::export_r_script(rv$results[[input$detail_var]],
                                 data_name = "my_data", file = file)
    }
  )

  output$dl_docx <- downloadHandler(
    filename = function() paste0("statarbor_report_", Sys.Date(), ".docx"),
    content = function(file) {
      valid_res <- rv$results[vapply(rv$results, inherits, logical(1), "statarbor_result")]
      req(length(valid_res) > 0)
      statarbor::export_word_report(
        results = valid_res,
        file = file,
        lang = input$lang,
        plot_type = if (!is.null(input$plot_type)) input$plot_type else "boxplot",
        font_family = if (!is.null(input$font_family)) input$font_family else "serif"
      )
    }
  )

  # --- TAB 3: CLD PLOTS ---
  output$ui_plot_var <- renderUI({
    req(length(rv$results) > 0)
    selectInput("plot_var", label = tr("plot_var"), choices = names(rv$results))
  })

  cld_plot_obj <- reactive({
    req(input$plot_var, rv$results[[input$plot_var]])
    plot(
      rv$results[[input$plot_var]],
      type = input$plot_type,
      show_letters = isTRUE(input$show_letters),
      font_family = input$font_family,
      base_size = input$font_size,
      x_angle = input$x_angle
    )
  })

  output$cld_plot <- renderPlot({ cld_plot_obj() })

  output$dl_plot <- downloadHandler(
    filename = function() paste0("statarbor_", input$plot_var, "_", input$plot_type, ".png"),
    content = function(file) {
      ggplot2::ggsave(file, plot = cld_plot_obj(), width = 22, height = 15,
                      units = "cm", dpi = as.numeric(input$plot_dpi), bg = "white")
    }
  )

  # --- TAB 4: ADVANCED MODELS ---
  output$ui_2g_group <- renderUI({
    req(rv$data)
    cols2 <- two_level_cols()
    validate(need(length(cols2) > 0, "No 2-level factor found in dataset."))
    selectInput("col_2g_grp", "2-Level Factor:", choices = cols2, selected = cols2[1])
  })

  output$ui_2g_resp <- renderUI({
    req(numeric_cols())
    selectInput("col_2g_resp", "Numeric Response:", choices = numeric_cols(), selected = numeric_cols()[1])
  })

  res_2g <- reactive({
    req(rv$data, input$col_2g_grp, input$col_2g_resp)
    tryCatch(
      statarbor::compare_two_groups(rv$data, response = input$col_2g_resp,
                                    group = input$col_2g_grp, mode = input$mode_2g),
      error = function(e) e$message
    )
  })

  output$out_2g <- renderPrint({
    r <- res_2g()
    if (is.character(r)) cat("Error:", r) else print(r)
  })

  output$plot_2g <- renderPlot({
    r <- res_2g()
    validate(need(inherits(r, "statarbor_two_group"), as.character(r)))
    df_p <- data.frame(
      g = as.factor(as.character(rv$data[[input$col_2g_grp]])),
      y = suppressWarnings(as.numeric(as.character(rv$data[[input$col_2g_resp]])))
    )
    df_p <- stats::na.omit(df_p)
    p_txt <- if (r$p_value < 0.001) "p < 0.001" else sprintf("p = %.4f", r$p_value)
    eff_txt <- sprintf("%s = %.3f", names(r$effect_size)[1], unlist(r$effect_size)[1])
    ggplot2::ggplot(df_p, ggplot2::aes(x = g, y = y, fill = g)) +
      ggplot2::geom_boxplot(alpha = 0.82, outlier.colour = "#D62728", outlier.shape = 8) +
      ggplot2::scale_fill_manual(values = c("#1B365D", "#D62728")) +
      ggplot2::labs(
        title = paste0(input$col_2g_resp, " by ", input$col_2g_grp),
        subtitle = sprintf("%s | %s = %.3f, %s, %s", r$method, r$statistic_name, r$statistic, p_txt, eff_txt),
        x = input$col_2g_grp, y = input$col_2g_resp
      ) +
      ggplot2::theme_minimal(base_size = 12, base_family = "serif") +
      ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5))
  })

  output$ui_bin_resp <- renderUI({
    req(rv$data)
    cols2 <- two_level_cols()
    validate(need(length(cols2) > 0, "No binary (2-level) outcome column found."))
    sel <- if ("germination" %in% cols2) "germination" else cols2[length(cols2)]
    selectInput("col_bin_resp", "Binary Outcome (0/1):", choices = cols2, selected = sel)
  })

  output$ui_bin_group <- renderUI({
    req(factor_cols())
    f_cols <- factor_cols()
    sel <- if (!is.null(rv$preferred_group) && rv$preferred_group %in% f_cols) rv$preferred_group else f_cols[1]
    selectInput("col_bin_grp", "Grouping Factor:", choices = f_cols, selected = sel)
  })

  res_bin <- reactive({
    req(rv$data, input$col_bin_resp, input$col_bin_grp)
    validate(need(input$col_bin_resp != input$col_bin_grp, "Select two different columns."))
    tryCatch(
      statarbor::analyze_binary(rv$data, response = input$col_bin_resp,
                                group = input$col_bin_grp, use_firth = input$firth_mode),
      error = function(e) e$message
    )
  })

  output$out_bin_summary <- renderPrint({
    r <- res_bin()
    if (is.character(r)) cat("Error:", r) else print(r)
  })

  output$tbl_bin_coef <- renderDT({
    r <- res_bin()
    validate(need(inherits(r, "statarbor_binary"), as.character(r)))
    DT::datatable(r$coefficients, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  output$ui_ord_resp <- renderUI({
    req(factor_cols())
    sel <- if ("viability_score" %in% factor_cols()) "viability_score" else factor_cols()[1]
    selectInput("col_ord_resp", "Ordered Outcome:", choices = factor_cols(), selected = sel)
  })

  output$ui_ord_group <- renderUI({
    req(factor_cols())
    f_cols <- factor_cols()
    sel <- if (!is.null(rv$preferred_group) && rv$preferred_group %in% f_cols) rv$preferred_group else f_cols[1]
    selectInput("col_ord_grp", "Grouping Factor:", choices = f_cols, selected = sel)
  })

  res_ord <- reactive({
    req(rv$data, input$col_ord_resp, input$col_ord_grp)
    validate(need(input$col_ord_resp != input$col_ord_grp, "Select two different columns."))
    tryCatch(
      statarbor::analyze_ordinal(rv$data, response = input$col_ord_resp, group = input$col_ord_grp),
      error = function(e) e$message
    )
  })

  output$out_ord_summary <- renderPrint({
    r <- res_ord()
    if (is.character(r)) cat("Error:", r) else print(r)
  })

  output$tbl_ord_coef <- renderDT({
    r <- res_ord()
    validate(need(inherits(r, "statarbor_ordinal"), as.character(r)))
    DT::datatable(r$coefficients, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  output$ui_glm_resp <- renderUI({
    req(numeric_cols())
    sel <- if ("fresh_weight_g" %in% numeric_cols()) "fresh_weight_g" else numeric_cols()[1]
    selectInput("col_glm_resp", "Numeric Response:", choices = numeric_cols(), selected = sel)
  })

  output$ui_glm_factors <- renderUI({
    req(factor_cols())
    def_f <- intersect(c("temperature_c", "ploidy"), factor_cols())
    if (length(def_f) < 2L) def_f <- utils::head(factor_cols(), 2)
    selectizeInput("col_glm_factors", "Factorial Predictors (>= 2):",
                   choices = factor_cols(), selected = def_f, multiple = TRUE)
  })

  res_glm <- reactive({
    req(rv$data, input$col_glm_resp, length(input$col_glm_factors) >= 2L)
    tryCatch(
      statarbor::analyze_interaction(rv$data, response = input$col_glm_resp,
                                     factors = input$col_glm_factors, family = input$glm_family),
      error = function(e) e$message
    )
  })

  output$out_glm_summary <- renderPrint({
    r <- res_glm()
    if (is.character(r)) cat("Error:", r) else print(r)
  })

  output$tbl_glm_anova <- renderDT({
    r <- res_glm()
    validate(need(inherits(r, "statarbor_interaction"), as.character(r)))
    DT::datatable(r$anova_table, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  # --- TAB 5: MULTIVARIATE ---
  output$ui_multi_vars <- renderUI({
    req(numeric_cols())
    selectizeInput("multi_vars", label = tr("multi_vars"),
                   choices = numeric_cols(),
                   selected = utils::head(numeric_cols(), 5), multiple = TRUE)
  })

  output$ui_pca_group <- renderUI({
    req(factor_cols())
    f_cols <- factor_cols()
    sel <- if (!is.null(rv$preferred_group) && rv$preferred_group %in% f_cols) rv$preferred_group else f_cols[1]
    selectInput("pca_group", label = tr("pca_grp"), choices = f_cols, selected = sel)
  })

  cor_obj <- reactive({
    req(rv$data, length(input$multi_vars) >= 2L)
    statarbor::plot_correlation(rv$data, vars = input$multi_vars, method = input$cor_method)
  })

  pca_obj <- reactive({
    req(rv$data, length(input$multi_vars) >= 2L, input$pca_group)
    statarbor::plot_pca(rv$data, vars = input$multi_vars, group = input$pca_group,
                        add_ellipses = isTRUE(input$pca_ellipse))
  })

  output$plot_cor <- renderPlot({ cor_obj() })
  output$plot_pca <- renderPlot({ pca_obj() })

  output$dl_cor <- downloadHandler(
    filename = function() paste0("statarbor_correlation_", input$cor_method, ".png"),
    content = function(file) ggplot2::ggsave(file, plot = cor_obj(), width = 20, height = 18, units = "cm", dpi = 300, bg = "white")
  )

  output$dl_pca <- downloadHandler(
    filename = function() "statarbor_pca_biplot.png",
    content = function(file) ggplot2::ggsave(file, plot = pca_obj(), width = 20, height = 18, units = "cm", dpi = 300, bg = "white")
  )

  # --- TAB 6: ABOUT ---
  output$about_content <- renderUI({
    if (input$lang == "tr") {
      tagList(
        h3("statarbor: Varsay\u0131m Denetimli \u0130statistiksel Karar Motoru"),
        p("statarbor; biyoloji, tar\u0131m ve deneysel bilimlerdeki ara\u015ft\u0131rmac\u0131lar i\u00e7in kod yazmadan varsay\u0131m denetimli hipotez testleri, \u00f6zel karakter korumal\u0131 harflendirme (CLD), Firth/Ordinal/GLM ileri modelleri, tekrarlanabilir R beti\u011fi ve yay\u0131na haz\u0131r Word (.docx) raporu sunan a\u00e7\u0131k kaynakl\u0131 bir R paketi ve web uygulamas\u0131d\u0131r."),
        h4("Lisans ve S\u00fcr\u00fcm"),
        p("Lisans: GNU GPL (>= 3) | S\u00fcr\u00fcm: 0.1.0")
      )
    } else {
      tagList(
        h3("statarbor: Assumption-Guided Statistical Decision Engine"),
        p("statarbor is an open-source R package and interactive web application designed for experimental researchers in agriculture, plant biotechnology, and life sciences. It automates assumption checking (Shapiro-Wilk, Levene, zero-variance screening), routes analyses across ANOVA/Tukey, Welch/Games-Howell, and Kruskal-Wallis/Dunn with special-character-safe CLD lettering, and exports reproducible R scripts, methods paragraphs, and formatted Word (.docx) reports."),
        h4("License & Version"),
        p("License: GNU GPL (>= 3) | Version: 0.1.0")
      )
    }
  })
}

shinyApp(ui = ui, server = server)
