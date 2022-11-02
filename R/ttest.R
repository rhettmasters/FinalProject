ttest <-  function(data){

  test <- stats::t.test(data[1], data[2], var.equal = TRUE)
  list("tvalue" = abs(test$statistic), "pvalue" = test$p.value, "conf_interval" = abs(test$conf.int), "estimate" = test$estimate)
}

