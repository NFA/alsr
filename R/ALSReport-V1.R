#' @include ALSReport-class.R
setMethod("als.version", "ALSReport-V1", function(object) "V1")

setMethod("parse_metadata", "ALSReport-V1", function(object) parse_metadata_v1(object))
setMethod("parse_tables", "ALSReport-V1", function(object) parse_tables_v1(object))

parse_metadata_v1 <- function(object) {
   table_text <- unlist(object@raw_page_data)

   header_start <- head(grep("Er beteckning", table_text) - 1L, n = 1L)
   table_text <- table_text[1:header_start]

   object@orderid <- stringr::str_match(table_text[1], "Rapport[:blank:]*([:alnum:]*)")[1,2] %>%
      readr::parse_character()


   match <- grep("Ankomstdatum", table_text, value = TRUE)
   if (length(match)) {
      arrived <- stringr::str_match(match, "Ankomstdatum ([0-9-]*)")
      object@arrived <- readr::parse_date(arrived[1, 2])
   }

   match <- grep("Utfärdad", table_text, value = TRUE)
   if (length(match)) {
      issued <- stringr::str_match(match, "Utfärdad[:blank:]* ([0-9-]*)")
      object@issued <- readr::parse_date(issued[1, 2])
   }

   match <- grep("Projekt", table_text, value = TRUE)
   if (length(match)) {
      project <- stringr::str_match(match, "Projekt[:blank:]* ([0-9]*)")
      object@project <- readr::parse_character(project[1, 2])
   }

   # match <- grep("Bestnr", table_text, value = TRUE)
   # if (length(match)) {
   #    project <- stringr::str_match(match, "Bestnr[:blank:]* ([0-9]*)")
   #    object@order_number <- readr::parse_character(project[1, 2])
   # }

   object@replacement <- any(grepl("Denna rapport med nummer .* ersätter tidigare utfärdad rapport", table_text))

   # Not metadata. Matches N tables
   match <- grep("Analys", table_text, value = TRUE)
   if (length(match)) {
      analysis <- stringr::str_match(match, "Analys(:?) (.*)")
      if (analysis[1, 2] == "") {
         object@analysis_v1 <- readr::parse_character(analysis[1, 1])
      } else {
         object@analysis_v1 <- readr::parse_character(analysis[1, 3])
      }
   }
   object
}

parse_tables_v1 <- function(object) {

   pages_with_reports <- grep("Er beteckning", object@raw_page_data)
   table_text <- unlist(object@raw_page_data[pages_with_reports])

   table_starts <- c(grep("Er beteckning", table_text), length(table_text) + 1L)


   tables <- list()
   for (n in 1:(length(table_starts) - 1L)) {
      tables[[n]] <- table_text[table_starts[n]:(table_starts[n + 1L] - 1L)]
   }

   for (n in 1:length(tables)) {
      if (grepl("_____", tables[n])) {
         table_end <- grep("_____", tables[[n]])
         tables[[n]] <- tables[[n]][1:(table_end - 1L)]
      }
   }

   cli::cli_alert_info("Parsing tables.")
   object@tables <- lapply(tables, parse_table_v1, object)
   object@samples <- unique(unlist(lapply(object@tables, "[[", "SampleID")))
   object@analyses <- unique(unlist(lapply(object@tables, "[[", "Analysis")))
   object
}

parse_table_v1 <- function(table_text, object) {
   # parse header
   # parse body
   header_end <- grep("Parameter", table_text) # expects table_text 1 cell, expected array??
   header_idxs <- 1:(header_end - 1L)

   header_text <- table_text[header_idxs]
   body_text <- table_text[-header_idxs]

   header <- parse_table_header_v1(header_text)
   body <- parse_table_body_v1(body_text)

   # add header info to body
   body %>%
   tibble::add_column(
      SampleID = header$sample_id,
      LabID    = header$lab_id,
      Matrix   = header$matrix,
      Date     = header$date,
      .before  = "Parameter") %>%
   tibble::add_column(
      Analysis = object@analysis_v1,
      .before = "Method"
   )
}

parse_table_header_v1 <- function(header_text) {

   header <- list(
      matrix = "",
      sample_id = "",
      lab_id = "",
      date = lubridate::NA_Date_
   )
   #cli::cli_alert_info("Parsing table header. Posssibly incomplete! (TODO) - Consume each header and report any residuals.")


   match <- grep("Er beteckning", header_text, value = TRUE)
   if(length(match)) {
      sample_id <- stringr::str_match(match, "Er beteckning[:blank:]*([:graph:]*)")
      header$sample_id <- readr::parse_character(sample_id[1, 2])
   }

   # match <- grep("Provtagare", header_text, value = TRUE)
   # if(length(match)) {
   #    date <- stringr::str_match(match, "Provtagare[:blank:]*([:graph:]*)")
   #    header$date <- readr::parse_date(date[1, 2], na = "ej")
   # }

   match <- grep("Labnummer", header_text, value = TRUE)
   if(length(match)) {
      lab_id <- stringr::str_match(match, "Labnummer[:blank:]*([:graph:]*)")
      header$lab_id <- readr::parse_character(lab_id[1, 2])
   }

   header
}

parse_table_body_v1 <- function(table_text) {

   # Remove unwanted characters
   table_text <- gsub("(\\(±\\)|\\*)", "", table_text)
   table_text <- stringr::str_trim(table_text)
   table_text <- table_text[table_text != ""]

   # Add space between Metod/Utf/Sign before sanitize
   table_text <- sanitize_input(table_text)
   table_body <- table_text[-1]
   column_header <- table_text[1]
   table_body <- column_correction(table_body)

   col_pos <- readr::fwf_empty(
      file = I(fwf_correction(column_header, table_body)),
      col_names = c("Parameter", "Result", "Uncertainty", "Unit", "Method", "Execution", "QA"))
   col_types <- readr::cols(
      Parameter   = readr::col_character(),
      Result      = readr::col_character(),
      Uncertainty = readr::col_character(),
      Unit        = readr::col_character(),
      Method      = readr::col_character(),
      Execution   = readr::col_character(),
      QA          = readr::col_character())

   # Unfortunetely readr::read_fwf tokenizes the text in C++ on a byte by byte
   # basis. Letters such as å, ä, ö in UTF-8 are encoded using multi-byte sequences.
   # Therefore the column positions may get left shifted when readr::read_fwf
   # incorectly counts, e.g. ö as two characters
   table <- tibble(
      Parameter   = stringr::str_sub(table_body, col_pos$begin[1], col_pos$end[1]),
      Result      = stringr::str_sub(table_body, col_pos$begin[2], col_pos$end[2]),
      Uncertainty = stringr::str_sub(table_body, col_pos$begin[3], col_pos$end[3]),
      Unit        = stringr::str_sub(table_body, col_pos$begin[4], col_pos$end[4]),
      Method      = stringr::str_sub(table_body, col_pos$begin[5], col_pos$end[5]),
      Execution   = stringr::str_sub(table_body, col_pos$begin[6], col_pos$end[6]),
      QA          = stringr::str_sub(table_body, col_pos$begin[7], -1),
   ) %>% dplyr::mutate(Result = stringr::str_squish(Result))

   nas <- c("ja", "Ja", "Nej", "nej", "----------", "Attached", "Bifogad", "Bilaga")
   table <- table %>%
      dplyr::mutate(Parameter       = readr::parse_character(Parameter)) %>%
      dplyr::mutate(BQL             = startsWith(Result, "<")) %>%
      dplyr::mutate(Result          = readr::parse_number(Result, na = nas),
                    Uncertainty     = readr::parse_number(Uncertainty),
                    Unit            = readr::parse_character(Unit),
                    Method          = readr::parse_character(Method),
                    Execution       = readr::parse_character(Execution),
                    QA              = readr::parse_character(QA)) %>%
      add_column(LOR = NA_real_, .before = "Method") %>%
      select(-c("Execution"))

   table
}
