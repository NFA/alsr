context("ALS V1 Report")
library(alsr)

alss <- c(
      "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/512H1DKEU1.pdf",
      "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/3B9OX97SP3.pdf",
      "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/3B9ND0XZWL.pdf",
      "H:/Documents/Papers/5_Pond sediment treatment/3_DataAnalysis/data/als/4EXZAL09RF.pdf")

test_that("successful loading of ALS V1 report file", {
   expect_equal(als.version(read_ALS(alss[[1]])), "V1")
   expect_equal(als.version(read_ALS(alss[[2]])), "V1")
   expect_equal(als.version(read_ALS(alss[[3]])), "V1")
   expect_equal(als.version(read_ALS(alss[[4]])), "V1")
})
