#---------------- Helper functions written to aid PanGB webapp.-----------------

#' Extract Data component from lists object
#'
#' This function extracts the Data component from
#' lists object generated using read_kasp_csv()
#'
#' @param x Results obtained from read_kasp_csv()
#' @return The 'Data' component in the kasp genotype file as a dataframe
#'
#' @examples
#' path1 <- system.file("extdata", "Genotyping_141.010_01.csv",
#'   package = "panGenomeBreedr",
#'   mustWork = TRUE
#' )
#'
#' file1 <- panGenomeBreedr::read_kasp_csv(file = path1, data_type = "raw")$Data
#' only_data <- get_data(file1)
#'
#' @export
#'
get_data <- function(x) {
  if (!"Data" %in% names(x)) {
    stop("Error: 'Data' element is missing from input list.")
  }
  data_only <- x[["Data"]] |> as.data.frame()

  return(data_only)
}
# get_data not needed



#' Add plates column with unique IDs
#'
#' This function adds plates column with unique ID's for kasp data file if not present.
#' This is later passed to the `nsamples_plate()` to obtain plate summary.
#'
#' @param x A data frame containing the data component from the read kasp file
#' @return A data frame with plate column added if not present
#'
#' @examples
#' # Data.with_plates <- plates_col(file1)
#' # Get plate summary from data.with_plates
#' # panGenomeBreedr::nsamples_plate(x = Data.with_plates,
#' #                               subset = 'plates',
#' #                               snp_id = 'SNPID',
#' #                               plate_id = 'MasterPlate')$summ
#'
#' @export
#' Valid!
plates_col <- function(x) {
  copied_data <- data.table::copy(x) # Copy the dataset

  colnames(copied_data) <- tolower(colnames(copied_data))

  # check if 'MasterPlate' and 'SNPID' exist
  if (!all(c("MasterPlate", "SNPID") %in% colnames(x))) {
    stop("Error: Columns 'MasterPlate' and 'SNPID' are missing in the input data.")
  }

  # Check if plates column exists
  if (!"plates" %in% colnames(x)) {
    x$plates <- paste(x$MasterPlate, x$SNPID, sep = "_")
  }

  return(x)
}

#' Get unique plates IDs
#'
#' This function extracts unique plates when plates column is added or already exists in Data
#'
#' @param x Results from read_kasp_csv()
#' @return A vector of unique plates IDs
#'
#' @examples
#' # uniq_plates(file1)
#'
#' @export
#'
uniq_plates <- function(x) {
  x_in <- plates_col(x)
  if (!"plates" %in% colnames(x_in)) {
    stop("Error: 'plates' column missing after applying plates_col().")
  }

  uniq_plate <- unique(x_in[["plates"]])

  return(uniq_plate) # return unique plate as a vector.
}



#' Generate a kasp marker map.
#'
#' This function parses SNPID strings and extracts chromosome and position details
#' and creates a mapping data frame
#' @param kasp_data a data frame of KASP  genotype calls for one or mulitiple plates
#'
#' @returns a data frame for kasp marker map
#'
#' @examples
#' data <- data.frame(SNPID = c("snpBS1000", "snpBS2000", "snpBS3000"))
#' kasp_marker_map(  kasp_data  = data)
#'
#' @export
#'
kasp_marker_map <- function(kasp_data) {
  # Validate input
  if (is.data.frame(kasp_data) != TRUE) {
    stop("Kasp_data must be a data frame")
  }

  # Confirm required column
  if (!("SNPID" %in% colnames(kasp_data))) {
    stop("Kasp_data must have column: SNPID")
  }

  # Index required column(SNPID)
  SNP_ids <- kasp_data[["SNPID"]] |> unique()

  # Get length of SNP_ids
  SNP_ids_length <- length(SNP_ids)

  # Initialize result dataframe
  kasp_map <- data.frame(
    SNPID = character(SNP_ids_length),
    SNP_ID2 = character(SNP_ids_length),
    chr = numeric(SNP_ids_length),
    pos = integer(SNP_ids_length),
    stringsAsFactors = FALSE
  )

  # for loop to generate and insert
  for (variable in seq_len(SNP_ids_length)) {
    # Insert value into snpid column
    kasp_map[variable, "SNPID"] <- SNP_ids[variable]

    # Insert value into snpid_2
    #- Split and format first
    formatted_ids <- stringr::str_match(
      string = SNP_ids[variable],
      pattern = "([a-zA-Z]+)(\\d+)"
    ) |> as.vector()

    #- if statement to  check and validate, also to crop of..
    if (SNP_ids[variable] %in% formatted_ids) {
      aa <- formatted_ids[formatted_ids != SNP_ids[variable]]

      kasp_map[variable, "SNP_ID2"] <- paste(aa[1], aa[2], sep = "|")
    } else {
      kasp_map[variable, "SNP_ID2"] <- paste(formatted_ids[1], formatted_ids[2], sep = "|") # inserted
    }

    # Insert value into chr column.
    kasp_map[variable, "chr"] <- variable

    # Insert position
    kasp_map[variable, "pos"] <- formatted_ids[length(formatted_ids)] |> as.integer()
  }

  return(kasp_map)
}




#' Extract genotype calls
#'
#' This function extracts all the genotype calls within the plates column
#'
#' @param x a list object processed by read_kasp_csv()
#' @param a an ID within the plates column
#'
#' @returns a vector of all genotype calls
#'
#' @examples
#' library(panGenomeBreedr)
#' path1 <- system.file("extdata", "Genotyping_141.010_01.csv",
#'   package = "panGenomeBreedr",
#'   mustWork = TRUE
#' )
#'
#' file1 <- read_kasp_csv(file = path1, data_type = "raw")
#' get_calls(x = file1, a = "SE-24-1088_P01_d2_snpSB00803")
#'
#' @export
#'
get_calls <- function(x, a) {
  x <- plates_col(x)

  if (!"plates" %in% colnames(x)) {
    stop("Error: 'plates' column is missing after applying plates_col().")
  }

  if (!"Call" %in% colnames(x)) {
    stop("Error: 'Call' column is missing in the dataset.")
  }

  x_filtered <- x[x[["plates"]] == a, ]

  if (nrow(x_filtered) == 0) {
    return(NULL) # Return NULL instead of causing a subscript error
  }

  return(x_filtered[["Call"]]) # Returns call column as a vector
}




#' Get colnames from KASP Data
#'
#' @param x a data frame of KASP genotype calls for one or multiple plates
#'
#' @returns a character vector
#'
#' @examples
#' library(panGenomeBreedr)
#' path1 <- system.file("extdata", "Genotyping_141.010_01.csv",
#'   package = "panGenomeBreedr",
#'   mustWork = TRUE
#' )
#'
#' file1 <- read_kasp_csv(file = path1, data_type = "raw")
#' col_names.data(file1)
#'
#' @export
#'
col_names.data <- function(x) {
  # Validate input
  if (!is.data.frame(x) && !is.matrix(x)) {
    stop("Input must be a data frame or matrix")
  }

  if (is.null(x) || nrow(x) == 0) {
    return(character(0)) # Return an empty character vector
  }

  return(colnames(x))
}



#' Convert genotype list to data frame
#'
#' This function takes a list of genotypes (typically from get_alleles()) and
#' converts it into a tabular data frame format for easier viewing and analysis.
#'
#' @param x List object with genotypes subsetted after get_alleles()
#' @return A data frame containing the genotype data with appropriate column names
#' @examples
#'
#' x <- sample(c("A:A", "A:-", "-:-", "Uncallable", "?"),
#'   size = 96,
#'   replace = TRUE
#' )
#'
#' # Assign NTC wells
#' x[c(88, 96)] <- "NTC"
#'
#' # Get alleles and expected genotypes
#' alleles <- get_alleles(x = x, data_type = "kasp")
#' genotypes(alleles$genotypes)
#'
#' @export
#'
genotypes <- function(x) {
  # Validate input
  if (!is.vector(x) && !is.list(x)) {
    stop("Input must be a vector or list")
  }

  # Unlist if needed
  x <- unlist(x)

  if (length(x) == 0) {
    warning("Input is empty")
    return(data.frame())
  }

  # Create a data frame with named columns
  result <- as.data.frame(matrix(
    nrow = 1, ncol = length(x),
    dimnames = list(NULL, names(x))
  ))

  # Assign all values at once
  result[1, ] <- x

  return(result)
}


#' Convert allele object to data frame
#'
#' This function transforms an allele object returned by get_alleles() into a
#' structured data frame format, with columns labeled as 'allele_A', 'allele_B', etc.
#'
#' @param x List object containing alleles from get_alleles()
#' @return A data frame with columns for each detected allele
#' @examples
#' alleles_df(alleles$alleles)
#'
#' @export
#'
alleles_df <- function(x) {
  # Validate input
  if (is.null(x)) {
    stop("Input cannot be NULL")
  }

  x <- unlist(x)

  if (length(x) == 0) {
    warning("No alleles found")
    return(data.frame())
  }

  # Create allele column names
  allele_cols <- paste("allele", LETTERS[1:length(x)], sep = "_")

  # Create and populate data frame
  df <- as.data.frame(matrix(
    nrow = 1,
    ncol = length(x),
    dimnames = list(NULL, allele_cols)
  ))
  df[1, ] <- x

  return(df)
}


#' Generate summary statistics for KASP genotyping plates
#'
#' This function analyzes KASP genotyping data and produces a summary report
#' on the status of each plate, including allele counts, locus type classification,
#' and overall success status.
#'
#' @param x Data frame containing KASP genotyping data
#' @param subset Column name containing plate identifiers (default: 'MasterPlate')
#' @param sep Separator character used in genotype calls (default: ':')
#' @param geno_call Column name containing genotype calls (default: 'Call')
#' @return A data frame summarizing each plate with columns for plate ID, allele count,
#'         locus type (Monomorphic, Bi-allelic, or Multi-allelic), and status (Successful or Failed)
#' @examples
#' # kasp_color_stat.give(file1$Data)
#' # kasp_color_stat.give(file1$Data, subset = "DaughterPlate", sep = ":", geno_call = "Call")
#'
#' @export
#'
kasp_color_stat.give <- function(x,
                                 subset = "MasterPlate",
                                 sep = ":",
                                 geno_call = "Call") {
  # Get unique plates
  plates <- x[, subset]
  plates_uniq <- unique(plates)
  nplates <- length(plates_uniq)

  # Create a dynamic data frame.
  dyn_df <- as.data.frame(matrix(nrow = nplates, ncol = 4, dimnames = list(NULL, c(subset, "Allele Count", "Locus Type", "Status"))))
  # Insert and populate.
  dyn_df[, 1] <- plates_uniq

  for (i in seq_len(nplates)) {
    # Subset each master plate
    master_plate <- x[plates == plates_uniq[i], ]

    # create a color vector based on KASP geno calls
    Color <- master_plate[[geno_call]]

    # Get alleles and possible genotypes using the `get_allele()` function

    alleles_geno <- get_alleles(
      x = Color,
      sep = sep,
      data_type = "kasp"
    )
    alleles <- alleles_geno$alleles

    dyn_df[i, 2] <- length(alleles)
    # if statements.
    if (length(alleles) == 2) {
      dyn_df[i, 3] <- "Bi-allelic"
      dyn_df[i, 4] <- "Successful"
    } else if (length(alleles) == 1) {
      dyn_df[i, 3] <- "Monomorphic"
      dyn_df[i, 4] <- "Successful"
    } else if (length(alleles) == 0) {
      dyn_df[i, 3] <- "None"
      dyn_df[i, 4] <- "Failed!"
    } else if (length(alleles) > 2) {
      dyn_df[i, 3] <- "Multi-allelic"
      dyn_df[i, 4] <- "Failed!"
    }
  }
  return(dyn_df)
}

#' Update column names in sample data frame
#'
#' This function dynamically renames columns in a sample data frame to match
#' specified identifiers for MasterPlate, SNP ID, and plate information.
#'
#' @param MasterPlate New name for the master plate column
#' @param SNIPID New name for the SNP ID column
#' @param PLATE New name for the plate column
#' @param data_nsamples Data frame containing sample information to be renamed
#' @return Data frame with updated column names
#' @examples
#' # new_colnames_nsamples("MP_ID", "SNP", "PlateNumber", sample_data)
#'
#' @export
#'
new_colnames_nsamples <- function(MasterPlate,
                                  SNIPID,
                                  PLATE,
                                  data_nsamples) {
  # Validate input
  if (!is.data.frame(data_nsamples)) {
    stop("Input must be a data frame")
  }

  if (ncol(data_nsamples) < 4) {
    stop("Input data frame must have at least 4 columns")
  }

  result <- as.data.frame(data_nsamples) # convert to dataframe

  # Update column names, preserving the 4th column name
  original_names <- colnames(result)
  new_names <- c(MasterPlate, SNIPID, PLATE, original_names[4:length(original_names)])
  colnames(result) <- new_names

  return(result)
}



#' Split user defined height and weight
#'
#' Converts a comma-separated string into a numeric vector of height and weight values.
#' The input string should contain exactly two numeric values separated by a comma.
#'
#' @param string A character string of length 1 containing two comma-separated numeric values
#'
#' @return A numeric vector of length 2, where the first element is height and the second is weight
#'
#' @examples
#' height_width( string= "8,10")
#' # Returns c(8, 5)
#'
#' @export
#'
height_width <- function(string){
  # Check if input is a character and length 1
  if(!is.character(string) || length(string) != 1) {
    stop("Input must be a single character string")
  }

  # Split the string by comma
  splitted <- strsplit(string, split = ',')[[1]]

  # Check if exactly two values were provided
  if(length(splitted) < 2) {
    stop("Input must contain exactly two values separated by a comma")
  }

  # Convert to numeric and handle potential NA values
  h_w_vector <- as.numeric(splitted)[1:2]

  if(any(is.na(h_w_vector))) {
    stop("Both values must be convertible to numbers")
  }

  return(h_w_vector)
}
