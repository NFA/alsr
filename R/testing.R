#library(alsr)



# files <- c(
#    # ALS V1
#    "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/512H1DKEU1.pdf",
#    "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/3B9OX97SP3.pdf",
#    "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/3B9ND0XZWL.pdf",
#    "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/4EXZAL09RF.pdf",
#    # ALS V2
#    "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/LE2003448_0_COA_Standard_sv-SE.pdf",
#    # GBA
#    "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/Attachment to L2020600.pdf",
#    "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/Attachment to L2023474.pdf",
#    "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/Bilaga till L2020480.pdf",
#    "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/Bilaga till L2023318.pdf"
#
# )
#
# problems <- c(
#    "H:/Documents/R/alsr/pdfs/S-1 In Organotins.pdf",
#    "H:/Documents/R/alsr/pdfs/S-1 Out Organotins.pdf"
# )
#
# als1 <- files[1]
# als2 <- files[5]
# gba1 <- files[6]

#read_ALS(gba1)

#test <- read_ALS("H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/38MH41A76X.pdf")

# strsplit(pdftools::pdf_text("H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/38MH41A76X.pdf"), "\r\n")
# strsplit(pdftools::pdf_text("H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/Attachment to L2020600.pdf"), "\n")
#
# x<- pdftools::pdf_text("H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/Attachment to L2020600.pdf")

#reports <- read_ALS(files)

#report <- read_ALS(files[[5]])


#detach("package:alsr", unload = TRUE)


# read_all_ALS <- function(path) {
#    als_files <- dir(path, pattern = "*.pdf", full.names = TRUE)
#
#    als_objects <- lapply(als_files, alsr::read_ALS)
#    als_tables <- lapply(als_objects, alsr::reports)
#
#    do.call("rbind", als_tables)
# }
#


