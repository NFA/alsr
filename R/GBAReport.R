#' @include ALSReport-class.R
setMethod("als.version", "GBAReport", function(object) "GBA")

setMethod("parse_metadata", "GBAReport", function(object) parse_metadata_gba(object))
setMethod("parse_tables", "GBAReport", function(object) parse_tables_gba(object))

parse_metadata_gba <- function(object) {
   meta_page <- object@raw_page_data[[1]]

   match <- grep("Test Report No.:", meta_page, value = TRUE)
   if (length(match)) {
      report_no <- stringr::str_match(match[1], "Test Report No.:[:blank:]*([:graph:]*)")
      object@gba_report <- readr::parse_character(report_no[1, 2])
   }

   match <- grep("Order", meta_page, value = TRUE)
   if (length(match)) {
      #print(match)
      gba_order <- stringr::str_match(match, "Order[:blank:]*(.*)")
      #print(gba_order)
      gba_order <- readr::parse_character(gba_order[1, 2])
      #print(gba_order)
      gba_order <- stringr::str_split(gba_order, " / ")
      #cat("Orders:")
      #print(gba_order)
      # This was last place I was editing
      object@gba_order <- unlist(gba_order)
   }

   match <- grep("GBA-No.", meta_page, value = TRUE)
   if (length(match)) {
      gba_no <- stringr::str_match(match, "GBA-No.[:blank:]*([:graph:]*)")
      object@gba_no <- readr::parse_character(gba_no[1, 2])
   }

   match <- grep("Date of arrival", meta_page, value = TRUE)
   if (length(match)) {
      arrived <- stringr::str_match(match, "Date of arrival[:blank:]*([:graph:]*)")
      object@arrived <- readr::parse_date(arrived[1, 2], format = "%d.%m.%Y")
   }
   object
}

parse_tables_gba <- function(object) {
   object
}

parse_table_gba <- function(table_text) {
   table_text
}

parse_table_header_gba <- function(header_text) {
   header_text
}

parse_table_body_gba <- function(table_text) {
   table_text
}
