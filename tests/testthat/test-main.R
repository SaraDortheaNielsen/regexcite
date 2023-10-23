test_that("All observations are included", {
  n = sample.int(100,1)
  data = data.frame(y=as.factor(rep(1,n)),x=rep(4,n))
  res = main(formula=y~x,data=data)
  expect_equal(length(res$y), n)
})
