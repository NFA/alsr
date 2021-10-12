# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Automatic reading of ALS reports in PDF-format
#'
#' \code{read_ALS} reads a an ALS report in PDF format and returns a S6 object
#' containing all the tables
#' @param file the name of the ALS report to be read from.
#' @export
read_ALS <- function(file) {
   if (length(file) != 1) {
      stop("Can only open one ALS report at a time.")
   }
   if (!file.exists(file)) {
      stop("File does not exist: ", file)
   }

   if (!validate_report(file)) {
      stop("File is not an ALS or (implemented) subcontractor report: ", file)
   }

   report <- create_report(file)
   report <- parse_metadata(report)
   report <- parse_tables(report)

   cli::cli_alert_success(stringr::str_glue("Parsed {length(report@tables)} table(s) from {basename(file)}."))
   return(report)
}

#' Returns all reports found in the ALS file in one data frame
#'
#' \code{reports(als)} Returns the reports in the parsed als object
#' @param alsreport the name of the ALS object to return the reports from
#' @export
reports <- function(alsreport) {

   if (attr(class(alsreport), "package") != "alsr") {
      stop("Object is not a valid ALS Report.")
   }

   tables <- alsreport@tables
   for(table in 1:length(tables)) {
      tables[[table]] %<>% tibble::add_column(File = alsreport@filename)
   }
   do.call("rbind", tables)
}

extract_pdf_data <- function(file, merge_tables) {
   extract_ALS_tables(file, merge = merge_tables)
}

