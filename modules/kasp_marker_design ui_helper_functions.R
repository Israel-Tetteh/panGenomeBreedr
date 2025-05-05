#' Extract Unique Genotype Codes from VCF File
#'
#' This function reads a VCF file and extracts all unique genotype codes present in the file.
#'
#' @param vcf_path Character string specifying the path to the VCF file.
#'
#' @return A character vector containing all unique genotype codes found in the file.
#'
#' @details The function uses the vcfR package to read the VCF file and extract the GT (genotype)
#' field from each variant position. It then collapses these into a unique set of codes.
#'
#' @examples
#' \dontrun{
#' geno_codes <- extract_geno_codes("path/to/your/file.vcf")
#' }
#'
#' @importFrom vcfR read.vcfR extract.gt
#'
extract_geno_codes <- function(vcf_path) {

  vcf_data <- vcfR::read.vcfR(vcf_path, verbose = FALSE) # read vcf file

  get_GT <- vcfR::extract.gt(x = vcf_data,
                             element = "GT",
                             as.numeric = FALSE
                             )

  geno_codes <- get_GT |>
    unlist() |>
    as.character() |>
    unique()

  return(geno_codes)

}

#' Extract Marker IDs and Chromosome IDs from VCF File
#'
#' This function reads a VCF file and extracts all unique marker IDs and chromosome IDs.
#'
#' @param vcf_path Character string specifying the path to the VCF file.
#'
#' @return A list with two elements:
#'   \item{vcf_matrix_markerID}{Character vector of unique marker IDs}
#'   \item{vcf_matrix_chromID}{Character vector of unique chromosome IDs}
#'
#' @details The function uses the vcfR package to read the VCF file and extracts the
#' marker IDs from the ID field and chromosome identifiers from the CHROM field in the VCF.
#'
#' @examples
#' \dontrun{
#' ids <- marker.chr_ID("path/to/your/file.vcf")
#' marker_ids <- ids$vcf_matrix_markerID
#' chrom_ids <- ids$vcf_matrix_chromID
#' }
#'
#' @importFrom vcfR read.vcfR
#'
marker.chr_ID <- function(vcf_path) {
  vcf_data <- vcfR::read.vcfR(vcf_path, verbose = FALSE)

  vcf_matrix <- as.matrix(vcf_data@fix) |> as.data.frame()

  vcf_matrix_chromID <- unique(vcf_matrix[["CHROM"]])
  vcf_matrix_markerID <- unique(vcf_matrix[["ID"]])

  result_list <- list(
    vcf_matrix_markerID = vcf_matrix_markerID,
    vcf_matrix_chromID = vcf_matrix_chromID
  )

  return(result_list)

}
