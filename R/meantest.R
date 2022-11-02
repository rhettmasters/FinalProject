meantest <- function(file_name, par1, par2){

  data <-  readr::read_csv(file_name, show_col_types = FALSE)
  p1 <-  deparse(substitute(par1))
  p2  <- deparse(substitute(par2))
  data1 <-  tidyr::pivot_wider(data, names_from = p2, values_from = p1)
  data1 <-  dplyr::select(data1, utils::tail(names(data1), 2))

  hypothesis(2, data1, p1, p2)

  assumptions(2, data1, p1, p2)

  testdata <- ttest(data1)

  cat("\n", "Results:", "\n", "T-value:",testdata$tvalue, "\n", "P-value:", testdata$pvalue, "\n", sep = "")
  cat("Confidence Interval:", testdata$conf_interval, sep =" ")

  decision(2, testdata, 0.05)

  conclusion(2, testdata, 0.05)

}

