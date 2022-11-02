decision <- function(testnum, data, sig){
  if(testnum == 1){
    if (data[[6]] <= sig){
      cat("\n \n",'P value is below the significance level of ', sig,"\n",'Therefore the result is statistically significant. Thus reject the null hypothesis ',sep = "")
    }
    else{
      cat('\n \n','P value is above the signifcance level of ', sig,"\n",'Therefore the result is statistically insignificant. Thus accept null hypothesiss ',sep = "")
    }
  }

  if (testnum==2){
    if (data$pvalue <= sig){
      cat("\n \n",'P value is below the significance level of ', sig,"\n",'Therefore the result is statistically significant. Thus reject the null hypothesis ', sep = "")
    }
    else{
      cat('\n \n','P value is above the signifcance level of ', sig,"\n", 'Therefore the result is statistically insignificant. Thus accept null hypothesis ',sep = "")
    }
  }

  if(testnum==3){
    if (data$pvalue <= sig){
      cat("\n \n",'P value is below the significance level of ', sig,"\n",'Therefore the result is statistically significant. Thus reject the null hypothesis ', sep = "")
    }
    else{
      cat('\n \n','P value is above the signifcance level of ', sig,"\n", 'Therefore the result is statistically insignificant. Thus accept null hypothesis ',sep = "")

  }

  }
}

