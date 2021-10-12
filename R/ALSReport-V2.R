#' @include ALSReport-class.R
setMethod("als.version", "ALSReport-V2", function(object) "V2")

setMethod("parse_metadata", "ALSReport-V2", function(object) parse_metadata_v2(object))
setMethod("parse_tables", "ALSReport-V2", function(object) parse_tables_v2(object))

parse_metadata_v2 <- function(object) {
   meta_page <- object@raw_page_data[[1]]

   match <- grep("Ordernummer:", meta_page, value = TRUE)
   if (length(match)) {
      orderid <- stringr::str_match(match, "Ordernummer[:blank:]:[:blank:]*([:alnum:]*)")
      object@orderid <- readr::parse_character(orderid[1, 2])
   } else {
      match <- grep("Ordernummer", meta_page)
      if (length(match)) {
         orderid <- stringr::str_match(meta_page[match + 1L], ":[:blank:]*([:alnum:]*)")
         object@orderid <- readr::parse_character(orderid[1, 2])
      }
   }

   match <- grep("Ankomstdatum, prover", meta_page, value = TRUE)
   if (length(match)) {
      arrived <- stringr::str_match(match, "Ankomstdatum, prover[:blank:]*:[:blank:]*([:graph:]*)")
      object@arrived <- readr::parse_date(arrived[1, 2])
   }

   match <- grep("Utfärdad", meta_page, value = TRUE)
   if (length(match)) {
      issued <- stringr::str_match(match, "Utfärdad[:blank:]*:[:blank:]*([:graph:]*)")
      object@issued <- readr::parse_date(issued[1, 2])
   }

   match <- grep("Projekt", meta_page, value = TRUE)
   if (length(match)) {
      project <- stringr::str_match(match, "Projekt[:blank:]*:[:blank:]*([:graph:]*)")
      object@project <- readr::parse_character(issued[1, 2])
   }

   object
}

parse_tables_v2 <- function(object) {
   # For V2 documents, first page is a company page
   table_text <- unlist(object@raw_page_data[-1])
   end <- grep("Metodsammanfattningar", table_text)
   table_text <- table_text[1:(end - 1L)]

   table_starts <- c(grep("Matris", table_text), length(table_text) + 1L)

   tables <- list()
   for (n in 1:(length(table_starts) - 1L)) {
      tables[[n]] <- table_text[table_starts[n]:(table_starts[n + 1L] - 1L)]
   }

   #cli::cli_alert_info("Parsing tables.")
   object@tables <- lapply(tables, parse_table_v2)
   object@samples <- unique(unlist(lapply(object@tables, "[[", "SampleID")))
   object@analyses <- unique(unlist(lapply(object@tables, "[[", "Analysis")))
   object
}

parse_table_v2 <- function(table_text) {
   #cli::cli_alert_info("Parsing table.")
   header_text <- table_text[1:3]
   table_text <- table_text[-(1:3)]

   # If table was last on page, it will contain page information
   page_header <- list(
      grepl("Sida", table_text),
      grepl("Ordernummer", table_text),
      grepl("Kund", table_text)
   )
   table_text <- table_text[!Reduce("|", page_header)]

   header <- parse_table_header_v2(header_text)
   body <- parse_table_body_v2(table_text)

   # add header info to body
   body %>% tibble::add_column(
      SampleID = header$sample_id,
      LabID    = header$lab_id,
      Matrix   = header$matrix,
      Date     = header$date,
      .before  = "Parameter")
}

parse_table_header_v2 <- function(header_text) {

   header <- list(
      matrix = "",
      sample_id = "",
      lab_id = "",
      date = ""
   )
   #cli::cli_alert_info("Parsing table header.")
   match <- grep("Matris:", header_text, value = TRUE)
   if(length(match)) {
      sample_matrix <- stringr::str_match(match, "Matris:[:blank:]*([:alnum:]*)")
      header$matrix <- readr::parse_character(sample_matrix[1, 2])
   }

   match <- grep("Provbeteckning", header_text, value = TRUE)
   if(length(match)) {
      sample_id <- stringr::str_match(match, "Provbeteckning[:blank:]*([:graph:]*)")
      header$sample_id <- readr::parse_character(sample_id[1, 2])
   }

   match <- grep("Laboratoriets provnummer", header_text, value = TRUE)
   if(length(match)) {
      lab_id <- stringr::str_match(match, "Laboratoriets provnummer[:blank:]*([:graph:]*)")
      header$lab_id <- readr::parse_character(lab_id[1, 2])
   }

   match <- grep("Provtagningsdatum / tid", header_text, value = TRUE)
   if(length(match)) {
      date <- stringr::str_match(match, "Provtagningsdatum / tid[:blank:]*([:graph:]*)")
      header$date <- readr::parse_date(date[1, 2], na = "ej")
   }

   header
}

parse_table_body_v2 <- function(table_text) {
   #cli::cli_alert_info("Parsing table body.")
   headings <- c("Petroleumkolväten", "Oorganiska parametrar",
                 "Näringsparametrar", "Provberedning",
                 "Metaller och grundämnen", "Uppslutning",
                 "Torrsubstans$")

   # Remove (sub)headings in table
   for (heading in headings) {
      match <- grep(heading, table_text)
      if (length(match)) {
         table_text <- table_text[-match]
      }
   }

   # Remove unwanted characters
   table_text <- gsub("±", "", table_text)
   table_text <- stringr::str_trim(table_text)
   table_text <- table_text[-1]

   # Sprinkling some extra spaces in special cases
   table_text <- stringr::str_replace_all(table_text, " ----", c(pattern1 = "---- "))
   table_text <- stringr::str_replace_all(table_text, "(TS-105 )", c(pattern1 = "\\1  "))
   table_text <- stringr::str_replace_all(table_text, "(OJ-20C )", c(pattern1 = "\\1  "))
   table_text <- stringr::str_replace_all(table_text, "(Närsalter_[:graph:]*)", c(pattern1 = "\\1  "))
   table_text <- stringr::str_replace_all(table_text, "(W-AFS-[:graph:]*)", c(pattern1 = "\\1  "))
   table_text <- stringr::str_replace_all(table_text, "(\\d) ([µm])", c(pattern1 = "\\1  \\2"))
   table_text <- stringr::str_replace_all(table_text, "[:blank:]{2,}", c(pattern1 = "\t"))

   # Remove empty lines and the column name row
   table_text <- table_text[table_text != ""][-1]

   # Column names and types
   col_names <- c("Parameter", "Result", "Uncertainty", "Unit", "LOR", "Analysis", "Method", "QA")
   col_types <- col_types <- readr::cols(
      Parameter   = readr::col_character(),
      Result      = readr::col_character(),
      Uncertainty = readr::col_character(),
      Unit        = readr::col_character(),
      LOR         = readr::col_number(),
      Analysis    = readr::col_character(),
      Method      = readr::col_character(),
      QA          = readr::col_character())


   table <- readr::read_tsv(file = I(table_text), col_names = col_names, col_types = col_types) %>%
      dplyr::mutate(BQL = startsWith(Result, "<")) %>%
      dplyr::mutate(Result = as.als_number(Result)) %>%
      dplyr::mutate(Uncertainty = readr::parse_double(Uncertainty, na = "----"))
   table
}
