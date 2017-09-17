
#' @get /machine
predict2 = function(one, two, three, four, five) {
  
  scoring <- function(x) {
    if (x[1] == 0 && x[4] == 0) {
      return (0)
    } else if (x[2] >= 5 && x[1] <= 2) {
      return(10)
    } else if (x[3] >= 2000) {
      return(90)
    } else if (x[4] >= 3) {
      return(30)
    } else if (x[5] > 1) {
      return(120)
    } else {
      y = round(runif(1, 20, 40))
      return(y)
    }
  }
  
  late_payment_count = sample(c(round(rnorm(200, 3)), round(rnorm(500, 5)), rep(0, 300)))
  credit_age = sample(c(round(rnorm(200, 3)), round(rnorm(300, 5)), round(rnorm(500, 10))))
  unpaid_account_rec = round(sample(c(rnorm(200, 2000), rnorm(150, 200), rnorm(300, 400), rnorm(50, 1000), rnorm(300, 200))), 2)
  overdue_payment_count = sample(c(round(rnorm(300, 3)), round(rnorm(200, 4)), round(rnorm(100, 5)), round(rnorm(100, 6)), rep(0, 300)))
  derogatory_mark_count = sample(c(rep(2,20), rep(0, 900), rep(1, 80)))
  
  intuit = data.frame(late_payment_count, credit_age, unpaid_account_rec, overdue_payment_count, derogatory_mark_count)
  a = intuit %>% mutate(avg_late_payment_days = apply(intuit, 1, function(x) scoring(x)))
  myControl = trainControl(method = "cv", number = 2, verboseIter = TRUE)
  rf_model = train(late_payment_count ~ ., data = a, trControl = myControl)
  a = data.frame(cbind(one, two, three, four, five))
  names(a) = c('late_payment_count', 'credit_age', 'unpaid_account_rec', 'overdue_payment_count', 'derogatory_mark_count')
  predict(rf_model, t)
}








