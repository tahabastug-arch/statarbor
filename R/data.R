#' Simulated Plant Thermal Stress Benchmark Dataset
#'
#' A synthetic biological stress-response dataset designed to demonstrate and
#' benchmark all statistical routing pathways in `statarbor`, including
#' parametric normal/homogeneous responses (`embryo_mm`), heteroscedastic
#' normal responses (`fresh_weight_g`), zero-variance lethal stress treatments
#' (`shoot_cm`, `root_cm`), ordered viability scores (`viability_score`), and
#' complete-separation binary germination outcomes (`germination`).
#'
#' @format A data frame with 120 rows and 10 variables:
#' \describe{
#'   \item{treatment}{Factor with 6 levels: `"Control"`, `"+4C_1W"`, `"+4C_2W"`,
#'     `"+4C_4W"`, `"0C_2W"`, and `"-20C_1W"`.}
#'   \item{temperature_c}{Factor indicating storage temperature in degrees
#'     Celsius (`"Control"`, `"4"`, `"0"`, `"-20"`).}
#'   \item{duration_wk}{Factor indicating storage duration in weeks (`"0"`,
#'     `"1"`, `"2"`, `"4"`).}
#'   \item{ploidy}{Two-level factor (`"Diploid"`, `"Haploid"`).}
#'   \item{embryo_mm}{Numeric initial embryo length in mm (normal, homogeneous
#'     variance).}
#'   \item{fresh_weight_g}{Numeric seedling fresh weight in g (strictly
#'     positive, normal with heterogeneous group variances).}
#'   \item{shoot_cm}{Numeric shoot length in cm (zero variance in lethal
#'     `"-20C_1W"` group).}
#'   \item{root_cm}{Numeric primary root length in cm.}
#'   \item{viability_score}{Integer ordered tissue viability score (`0` to `3`).}
#'   \item{germination}{Integer binary germination indicator (`0` = no, `1` = yes)
#'     exhibiting complete separation across extreme treatments.}
#' }
#' @source Simulated synthetic benchmark data generated for `statarbor`.
#' @examples
#' data(thermal_stress)
#' auto_compare(thermal_stress, "embryo_mm", "treatment")
#' auto_compare(thermal_stress, "fresh_weight_g", "treatment")
#' auto_compare(thermal_stress, "shoot_cm", "treatment")
"thermal_stress"
