# alsr - Import laboratory reports from ALS Scandinavia AB easy

## Supported formats
Two types of reports are supported. They are in this package internally referred to as v1 and v2. The reports in v1 were used until sometime in 2020 when v2 format was introduced. They differ somewhat in layout and structure.

Reports from subcontractors are currently not supported. 

## Installation

Use a package that can install from Github. For example remotes.

```r
install.packages("remotes")
```
Then you can install alsr using:
```r
remotes::install_github("NFA/alsr")
```

## Usage

Parsing a PDF report:
```r
> library(alsr)
> data <- read_ALS("report.pdf")
i ALS V1 report.
i Parsing tables.
√ Parsed 3 table(s) from report.pdf.

> data
# ALS Report (V1)
# Filename: report.pdf
# Order ID: L2022374
# Arrived: 2020-10-26
# Issued: 2020-11-11
# Project: 1773270
# Analyses: M2-VB
# Samples: N15-Sludge, N15-C-Cake and N15-T-Cake
# Number of tables: 3
```

Objects are represented using the S4 class structure. Each sample analyzed is parsed into a separate table. The tables are stored in the tables `slot` as a list of data frames. The tables `slot` can be accessed using the `@` operator.
```r
> data@tables
# prints out the list of tables
> data@tables[[1]]
# prints out the first table
```

Alternatively, the helper function `reports` may be used to return all tables (samples) put together into one single data frame.

```r
> reports(data)
# prints out all tables in one 
```

If you want to read all the PDFs in a certain directory the following little function may be of use:

```r
read_all_ALS <- function(path) {
   als_files <- dir(path, pattern = "*.pdf", full.names = TRUE)
   
   als_objects <- lapply(als_files, alsr::read_ALS)
   als_tables <- lapply(als_objects, alsr::reports)
   
   do.call("rbind", als_tables)
}
```
## Misc. information
When a result for a parameter is below the reporting limit the numerical part of the result is parsed into the Result column and the BQL (below quantification limit) column is set to `TRUE`. For example, a result `<1`, the number `1` is parsed into Result, and BQL set to `TRUE`.

If the report is an replacement for an earlier report the
replacement `slot` is set to true. However, there is no further logic to discard the previous report, this you will have to curate manually.

```r
> data@replacement
[1] FALSE
```