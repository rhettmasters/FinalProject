ctest <- function(data, par1, par2){
  results <- stats::chisq.test(data[[par1]], data[[par2]])
  list("xsq" = results$statistic[[1]], "pvalue" = results$p.value[[1]], "df" = results$parameter[[1]])
}
