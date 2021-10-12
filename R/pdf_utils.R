

extract_ALS_tables <- function(file, merge, ...) {
   page_texts <- pdftools::pdf_text(file)
   # Metadata exists in the page header and consists of order_id and document_id
   metadata <- extract_metadata(page_texts[1])
   table_lists <- lapply(page_texts, extract_tables, ...)
   table_lists <- table_lists[!is.na(table_lists)]
   table_lists <- unlist(table_lists, recursive = FALSE)
   table_lists <- lapply(table_lists, merge_metadata, metadata = metadata)
   #table_lists <- lapply(table_lists, unname)

   info <- stringr::str_glue("Read {length(table_lists)} table{?s} from {file}.")
   cli::cat_alert_info(info)

   if (merge) {
      table <- do.call("rbind", table_lists)
      return(table)
   }

   return(table_lists)
}

extract_metadata <- function(table_text) {
   table_text <- unlist(strsplit(table_text, "\r\n"))

   header_start <- head(grep("Er beteckning", table_text) - 1L, n = 1L)
   table_text <- table_text[1:header_start]

   metadata <- list(
      order_id = "",
      arrived = "1970-01-01",
      issued = "1970-01-01",
      project = "NA",
      order_number = "NA",
      analysis = "NA",
      replacement = FALSE
   )


   metadata$order_id <- stringr::str_match(table_text[1], "Rapport[:blank:]*([:alnum:]*)")[1,2] %>%
     readr::parse_character()

   # match <- stringr::str_extract(c(table_text[2], table_text[3]), "[A-Z0-9]{9,11}")
   # metadata$document_id <- match[!is.na(match)][1] %>%
   #    readr::parse_character()


   match <- grep("Ankomstdatum", table_text, value = TRUE)
   if (length(match)) {
      arrived <- stringr::str_match(match, "Ankomstdatum ([0-9-]*)")
      metadata$arrived <- readr::parse_date(arrived[1, 2])
   }

   match <- grep("Utfärdad", table_text, value = TRUE)
   if (length(match)) {
      issued <- stringr::str_match(match, "Utfärdad[:blank:]* ([0-9-]*)")
      metadata$issued <- readr::parse_date(issued[1, 2])
   }

   match <- grep("Projekt", table_text, value = TRUE)
   if (length(match)) {
      project <- stringr::str_match(match, "Projekt[:blank:]* ([0-9]*)")
      metadata$project <- readr::parse_character(project[1, 2])
   }

   match <- grep("Bestnr", table_text, value = TRUE)
   if (length(match)) {
      project <- stringr::str_match(match, "Bestnr[:blank:]* ([0-9]*)")
      metadata$order_number <- readr::parse_character(project[1, 2])
   }

   metadata$replacement <- any(grepl("Denna rapport med nummer .* ersätter tidigare utfärdad rapport", table_text))

   match <- grep("Analys", table_text, value = TRUE)
   if (length(match)) {
      analysis <- stringr::str_match(match, "Analys(:?) (.*)")
      if (analysis[1, 2] == "") {
         metadata$analysis <- readr::parse_character(analysis[1, 1])
      } else {
         metadata$analysis <- readr::parse_character(analysis[1, 3])
      }
   }

   return(metadata)
}

extract_tables <- function(page_text, ...) {
   page_text <- unlist(strsplit(page_text, "\r\n"))
   page_data <- list(page = 0, pages = 0)

   match <- grep("Sida", page_text, value = TRUE)
   if (length(match)) {
      pages <- stringr::str_match(match, "Sida (\\d+) \\((\\d+)\\)")
      page_data$page  <- readr::parse_number(pages[1, 2])
      page_data$pages <- readr::parse_number(pages[1, 3])
   }

   table_idx <- grep("Er beteckning", page_text)
   if (length(table_idx)) {
      # The page contains tables. Remove the page header and footer
      page_header <- seq(1, table_idx[1] - 1)
      page_footer <- seq(grep("_________", page_text), length(page_text))
      page_text <- page_text[-c(page_header, page_footer)]

      if (grepl("Rapporteringsgränsen", page_text[length(page_text)])) {
         page_text <- page_text[-length(page_text)]
      }

      # page may contain one or more tables, split them up
      table_texts <- split_tables(page_text)
      table_texts <- lapply(table_texts, extract_result_table)
      table_texts <- lapply(table_texts, add_page_info, page_data)
      return(table_texts)
   } else {
      #cat("No result table on page", page_data$page, "of", page_data$pages, ".\n")

      return(NA)
      # no result table
   }


}

extract_result_table <- function(table_text, ...) {
   table_text <- unlist(strsplit(table_text, "\r\n"))

   # Extract the table header part and process it
   header_start <- grep("Er beteckning", table_text)
   header_end   <- grep("Parameter", table_text) - 1L
   if (!length(header_start) | !length(header_end)) {
      return(NA)
   }

   header_text <- table_text[seq(header_start, header_end)]
   header <- extract_table_header(header_text)

   # Each result table starts with Parameter and ends with a hline. Row with
   # Parameter is kept as it supports the fixed width import later.
   table_start <- grep("Parameter", table_text)
   table_end   <- length(table_text)
   table_text  <- table_text[table_start:table_end]

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
      file = fwf_correction(column_header, table_body),
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

   nas <- c("ja", "Ja", "Nej", "nej", "----------", "Attached", "Bifogad")
   table <- table %>%
      dplyr::mutate(Parameter       = readr::parse_character(Parameter)) %>%
      dplyr::mutate(BQL             = startsWith(Result, "<")) %>%
      dplyr::mutate(Result          = readr::parse_number(Result, na = nas),
                    Uncertainty     = readr::parse_number(Uncertainty),
                    Unit            = readr::parse_character(Unit),
                    Method          = readr::parse_integer(Method),
                    Execution       = readr::parse_character(Execution),
                    QA              = readr::parse_character(QA)) %>%
      tibble::add_column(AnalysisID = readr::parse_character(header$LabID),
                         Date       = readr::parse_date(header$Date),
                         Sampler    = readr::parse_character(header$Sampler),
                         Sample     = readr::parse_character(header$Sample),
                         Info       = readr::parse_character(header$Info),
                         .before    = "Parameter")

   #structure(table, class = append(class(table), "als_result_table"))
   table
}

extract_table_header <- function(header_text) {
   header <- list(
      Sample = "sample_name",
      Info = "NA",
      Sampler = "sampler",
      Date = "NA",
      LabID = "unknown"
   )

   header_text <- stringr::str_squish(header_text)
   header_text <- header_text[header_text != ""]

   extract_from_pattern <- function(string, pattern) {
      match <- stringr::str_subset(string, pattern)
      if (length(match)) {
         extract <- stringr::str_match(match, pattern)
         if (ncol(extract) == 2) {
            return(extract[1, 2])
         }
      }
      return(NA)
   }

   match <- extract_from_pattern(header_text, "Er beteckning (.*)")
   if (!is.na(match)) { header$Sample = match }

   match <- extract_from_pattern(header_text, "Provtagare (.*)")
   if (!is.na(match)) { header$Sampler = match }

   match <- extract_from_pattern(header_text, "Provtagningsdatum (.*)")
   if (!is.na(match)) { header$Date = match }

   match <- extract_from_pattern(header_text, "Labnummer (.*)")
   if (!is.na(match)) { header$LabID = match }

   # Sometimes there is an additional field containing sample info
   std_fields <- "(Er beteckning|Provtagare|Provtagningsdatum|Labnummer)"
   info <- stringr::str_subset(header_text, std_fields, negate = TRUE)
   if (length(info)) { header$Info = info[1] }

   return(header)
}

merge_metadata <- function(table, metadata) {
   table <- table %>% tibble::add_column(
      OrderID       = metadata$order_id,
      IsReplacement = metadata$replacement,
      Arrived       = metadata$arrived,
      Issued        = metadata$arrived,
      Project       = metadata$project,
      OrderNumber   = metadata$order_number,
      Analysis      = metadata$analysis,
      .before       = "Page"
   )
   return(table)
}

add_page_info <- function(table, page_data) {
   table <- table %>% tibble::add_column(
      Page = page_data$page,
      Pages = page_data$pages,
      .before = "AnalysisID"
   )
   return(table)
}
