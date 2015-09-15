
#' Indian ocean albacore
#'
#' A dataset containing the catch and catch per unit effort (CPUE) data for Indian Ocean albacore between 1967 and 1989.
#'
#' @format A data frame with 23 rows and 2 variables:
#' \describe{
#'   \item{catch}{catch in thousand tonnes}
#'   \item{cpue}{catch per unit effort in kilograms per hundred hooks}
#' }
#' @source Polacheck, T., R. Hilborn, and A.E. Punt, Fitting surplus production models: comparing models and measuring uncertainty. Canadian Journal of Fisheries and Aquatic Sciences, 1993. 50(12): p. 2597-2607.
"albio"

#' New Zealand hake
#'
#' A dataset containing the catch, catch per unit effort (CPUE) and trawl survey abundance data for hake from New Zealand (Chatham Rise stock) between 1975 and 2012.
#'
#' @format A data frame with 38 rows and 6 variables:
#' \describe{
#'   \item{year}{fishing year (i.e. 1975 is from September 1974 to August 1975)}
#'   \item{landings}{estimated landings in tonnes}
#'   \item{survey}{research survey biomass in tonnes}
#'   \item{survey_cv}{estimated research survey index coefficient of variation}
#'   \item{cpue}{standardised CPUE in tonnes per tow from the Eastern Chatham Rise}
#'   \item{cpue_cv}{estimated CPUE coefficient of variation from the standardisation}
#' }
#' @source Horn, P.L., Stock assessment of hake (Merluccius australis) on the Chatham Rise (HAK 4) and off the west coast of South Island (HAK 7) for the 2012-13 fishing year. New Zealand Fisheries Assessment Report, 2013/31: 58 p.
"haknz"