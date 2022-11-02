lmtest <-  function(file_name, par1, par2){

   data = readr::read_csv(file_name, show_col_types = FALSE)
   p1 <-  substitute(par1)
   p2  <- substitute(par2)

   hypothesis(1, data, p1, p2)

   assumptions(1, data, p1, p2)

   testdata <- fit(data, p1, p2)

   cat("\n", "Results:","\n",
       "Estimated slope:", testdata[[1]], "\n",
       "95% CI:", testdata[[2]], ",",testdata[[3]], "\n",
       "T-value:", testdata[[4]], "\n",
       "Degrees of Freedom:", testdata[[5]], "\n",
       "P-value:", testdata[[6]],"\n")

   decision(1, testdata, 0.05)

   conclusion(1, testdata, 0.05)


}
