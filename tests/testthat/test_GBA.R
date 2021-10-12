context("GBA Report")
library(alsr)

gbas <- c(
   "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/Attachment to L2020600.pdf",
   "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/Attachment to L2023474.pdf",
   "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/Bilaga till L2020480.pdf",
   "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/Bilaga till L2023318.pdf"
   )

test_that("successful loading of GBA report file", {
   expect_equal(gba.version(read_ALS(gbas[[1]])), "GBA standard")
   expect_equal(gba.version(read_ALS(gbas[[2]])), "GBA standard")
   expect_equal(gba.version(read_ALS(gbas[[3]])), "GBA standard")
   expect_equal(gba.version(read_ALS(gbas[[4]])), "GBA standard")
})
