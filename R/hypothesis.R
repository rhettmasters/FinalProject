hypothesis <- function(testnum, data, parameter1, parameter2){
  if(testnum== 1){
    cat("We are testing if there is a linear relationship between ", parameter1," & ",parameter2," i.e.","\n", sep = "")
    cat("Null Hypothesis: \U03B2 \U003D 0","Alternative Hypothesis: \U03B2 \U2260 0 ", sep = '\n')
  }
  if(testnum== 2){
    cat("We are testing whether the means of", colnames(data[1]), parameter1,"&",colnames(data[2]), parameter1, "are equal i.e.","\n", sep = " ")
    cat("Null Hypothesis: \U03BC_1 \U003D \U03BC_2","Alternative Hypothesis: \U03BC_1 \U2260 \U03BC_2  ", sep = '\n')
  }
  if(testnum==3){
    cat("We are testing whether there is any association between the variables", parameter1," & ",parameter2, " i.e.", sep = "")
    cat("\nNull Hypothesis: The 2 variables are independant","Alternative Hypothesis: not the null hypothesis ", sep = '\n')
  }
}

