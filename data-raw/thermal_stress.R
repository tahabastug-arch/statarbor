set.seed(2026)
z <- stats::qnorm(stats::ppoints(20))
grps <- c("Control", "+4C_1W", "+4C_2W", "+4C_4W", "0C_2W", "-20C_1W")
n_g <- 20L

# 1. Normal + homojen varyans -> ANOVA + Tukey
emb_means <- c(2.72, 2.68, 2.66, 2.64, 2.69, 2.65)
embryo_mm <- unlist(lapply(emb_means, function(m) round(m + 0.16 * sample(z), 3)))

# 2. Normal + heterojen varyans (> 0) -> Welch ANOVA & Gamma-Log GLM
fw_means <- c(0.85, 0.64, 0.36, 0.14, 0.30, 0.06)
fw_sds   <- c(0.22, 0.16, 0.10, 0.03, 0.08, 0.01)
fresh_weight_g <- unlist(Map(function(m, s) round(m + s * sample(z), 4), fw_means, fw_sds))

# 3. Sifir varyansli olumcul grup -> Kruskal-Wallis + Dunn
shoot_cm <- c(
  round(pmax(0.5, rnorm(n_g, 11.2, 2.2)), 2),
  round(pmax(0.2, rnorm(n_g, 7.8, 1.9)), 2),
  round(pmax(0.0, rnorm(n_g, 3.4, 1.3)), 2),
  round(pmax(0.0, rnorm(n_g, 0.8, 0.6)), 2),
  round(pmax(0.0, rnorm(n_g, 2.9, 1.1)), 2),
  rep(0, n_g)
)
root_cm <- round(pmax(0, shoot_cm * runif(length(shoot_cm), 0.45, 0.72) +
                        ifelse(shoot_cm > 0, rnorm(length(shoot_cm), 0, 0.25), 0)), 2)

# 4. Sirali canlilik skoru (0-3) -> Ordinal CLM
viability_score <- c(
  c(rep(3L, 16), rep(2L, 3), rep(1L, 1)),
  c(rep(3L, 13), rep(2L, 5), rep(1L, 2)),
  c(rep(3L, 6),  rep(2L, 8), rep(1L, 5), rep(0L, 1)),
  c(rep(2L, 3),  rep(1L, 9), rep(0L, 8)),
  c(rep(3L, 4),  rep(2L, 8), rep(1L, 6), rep(0L, 2)),
  c(rep(1L, 2),  rep(0L, 18))
)

# 5. Tam ayrilmali ikili cimlenme (%100 Kontrol, %0 -20C_1W) -> Firth Lojistik
germination <- c(
  rep(1L, n_g),
  c(rep(1L, 18), rep(0L, 2)),
  c(rep(1L, 14), rep(0L, 6)),
  c(rep(1L, 6),  rep(0L, 14)),
  c(rep(1L, 12), rep(0L, 8)),
  rep(0L, n_g)
)

thermal_stress <- data.frame(
  treatment = factor(rep(grps, each = n_g), levels = grps),
  temperature_c = factor(rep(c("Control", "4", "4", "4", "0", "-20"), each = n_g),
                         levels = c("Control", "4", "0", "-20")),
  duration_wk = factor(rep(c("0", "1", "2", "4", "2", "1"), each = n_g),
                       levels = c("0", "1", "2", "4")),
  ploidy = factor(rep(c("Diploid", "Haploid"), times = length(shoot_cm) / 2)),
  embryo_mm = embryo_mm,
  fresh_weight_g = fresh_weight_g,
  shoot_cm = shoot_cm,
  root_cm = root_cm,
  viability_score = viability_score,
  germination = germination,
  stringsAsFactors = FALSE
)

usethis::use_data(thermal_stress, overwrite = TRUE)
