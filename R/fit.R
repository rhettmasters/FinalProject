fit <- function(data, par1, par2){
  p1 = data[deparse(par1)]
  p2 = data[deparse(par2)]
  lm <- lm(formula = as.matrix(p1)~as.matrix(p2), data = data) #runs the linear model
  sum <- summary(lm) #summarise linear model
  list <- list( sum$coefficients[2,1],
                sum$coefficients[2,1] - 1.96*sum$coefficients[2,2],
                sum$coefficients[2,1] + 1.96*sum$coefficients[2,2],
                sum$coefficients[2,3],
                sum$df[2],
                sum$coefficients[2,4],
                'p1' = colnames(p1), 'p2' = colnames(p2))
  list
}
