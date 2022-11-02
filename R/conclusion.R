conclusion <- function(testnum, data, sig){
  if(testnum == 1){
   if (data[[6]] <= sig){
    cat('\n \n', 'As the P value is below the required significance level of ', sig, 'There is sufficient evidence to  suggest that there exists a significant linear relationship between',data[[7]]," and ",data[[8]], sep = "")
     }
    else{
    cat('\n \n','As the P value is not below the required signifcance level of', sig, 'There is insufficient evidence to suggest that a significant linear relationship exists between', data[[7]]," and ",data[[8]], sep = "")
    }
  }

   if (testnum==2){
    if (data$pvalue <= sig){
        cat("\n \n",'As the P-value is below the significance level of ', sig, ' there is signficant evidence to suggest a difference in mean between the two populations', sep = "")
        }
    else{
        cat('\n \n,','As the P-value is above the signifcance level of ', sig, ' there is insignficant evidence to suggest a difference in mean between the two populations',sep = "")
        }
    }

    if(testnum==3){
      if (data$pvalue <= sig){
        cat("\n \n",'As the P-value is below the significance level of ', sig, ' there is sufficient evidence to suggest that the variables are not independent',"\n", 'Thus a relationship exists between them', sep = "")
      }
      else{
        cat('\n \n','As the P-value is above the signifcance level of ', sig, ' there is insufficient evidence to suggest that the variables are not independent',"\n", 'Thus a relationship does not exists between them',sep = "")
      }
  }

}

