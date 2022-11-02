chistest <- function(file_name, par1, par2){
  data = readr::read_csv(file_name, show_col_types = FALSE)
  p1 <-  deparse(substitute(par1))
  p2  <- deparse(substitute(par2))

  hypothesis(3, data, p1, p2)

  assumptions(3,data, p1, p2)

  testdata <- ctest(data, p1, p2)

  cat("\n \n", "Results:","\n",
      "X Squared:", testdata[[1]], "\n",
      "P-Value:", testdata[[2]], ",",testdata[[3]], "\n",
      "Degrees of Freedom:","\n", sep="")

  decision(3, testdata, 0.05)

  conclusion(3, testdata, 0.05)


}
