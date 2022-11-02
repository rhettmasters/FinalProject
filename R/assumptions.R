assumptions <- function(testnum, data, par1, par2){
  if(testnum == 1){
    p1 = data[deparse(par1)]
    p2 = data[deparse(par2)]
    lm <- lm(formula = as.matrix(p1)~as.matrix(p2), data = data) #runs the linear model
    par(mfrow=c(2,2)) #so that my graphs can be displayed together
    plot(unlist(p1), unlist(p2), main = paste(colnames(p1), "vs", colnames(p2), sep= " "), xlab = colnames(p1), ylab = colnames(p2))
    plot(lm$residuals, lm$fitted.values, main = "Regression Residuals vs Fitted Values",xlab = 'Regression Residuals',ylab = 'Fitted Values')
    hist(lm$residuals, main = "Histogram of Regression Residuals", xlab = "Residuals")
    qqnorm(lm$residuals, main = "residual normal Q-Q plot")
  }
  if(testnum==2){
    graphics::par(mfrow = c(1,2))
    graphics::hist(unlist(data[1]), xlab = "", main = paste("Distribution of", colnames(data[1]), par1, sep = " "))
    graphics::hist(unlist(data[2]), xlab = "", main = paste("Distribution of", colnames(data[2]), par1, sep = " "))

  }
  if(testnum==3){
    cat("\n", "Verify all E(x) >= 5","\n","Table of E(x):","\n \n", sep = "")
    output <- stats::chisq.test(data[[par1]], data[[par2]])
    print(output$expected)
  }
}

