#' @title beta_carotene
#' @description A sample trait-predictive KASP genotype data for marker QC visualization.
#' @docType data
#' @usage data(beta_carotene)
#' @format A data frame with 768 observations and 11 variables:
#' \describe{
#'   \item{\code{DaughterPlate}}{\code{character} KASP Daughter Plate ID.}
#'   \item{\code{MasterPlate}}{\code{character} KASP Master Plate ID.}
#'   \item{\code{MasterWell}}{\code{character} KASP Master Well ID.}
#'   \item{\code{Call}}{\code{character} KASP observed genotype calls.}
#'   \item{\code{X}}{\code{double} FAM fluorescence values.}
#'   \item{\code{Y}}{\code{double} HEX fluorescence values.}
#'   \item{\code{SNPID}}{\code{character} KASP SNP ID.}
#'   \item{\code{SubjectID}}{\code{character} KASP Subject ID.}
#'   \item{\code{DaughterWell}}{\code{character} KASP Daughter Well ID.}
#'   \item{\code{Group}}{\code{character} Predicted genotype for positive controls.}
#'   \item{\code{plates}}{\code{character} Derived plate IDs for each 96-well KASP reaction plate.}
#'
#' }
"beta_carotene"
