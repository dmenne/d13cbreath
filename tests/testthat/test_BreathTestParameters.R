context("Test of breath test parameter")


test_that("Breath test parameter must return valid data or vectors",{
  
  cf = c(k = 0.576, beta = 5.24,dontcare = 4)
  df = data.frame(
             method = rep(c("NLS", "Bayesian")),  
             k = c(0.576,0.606,0.529,0.608), 
             beta = c(5.24,5.79,5.95,7.54))
  dff = df
  # multiple inheritance
  class(dff) = c("data.frame","dummy")
  
  expect_that(CumExpBeta(seq(0, 6, by = 0.5), 100, cf)[13] > 98.967, is_true())
  expect_error(CumExpBeta(seq(0, 6, by = 0.5), 100, df) ,"requires")
  
  expect_that(t50BluckCoward(cf), is_a("numeric") )
  expect_that(t50BluckCoward(df), is_a("numeric") )
  expect_that(t50BluckCoward(dff), is_a("numeric") )
  
  expect_that(tLagBluckCoward(cf), is_a("numeric") )
  expect_that(tLagBluckCoward(df), is_a("numeric") )
  expect_that(tLagBluckCoward(dff), is_a("numeric") )
  
  expect_that(t50Maes(cf), is_a("numeric") )
  expect_that(t50Maes(df), is_a("numeric") )
  expect_that(t50Maes(dff), is_a("numeric") )
  
  expect_that(tLagMaes(cf), is_a("numeric") )
  expect_that(tLagMaes(df), is_a("numeric") )
  expect_that(tLagMaes(dff), is_a("numeric") )
  
  expect_that(t50MaesScintigraphy(cf), is_a("numeric") )
  expect_that(t50MaesScintigraphy(df), is_a("numeric") )
  expect_that(t50MaesScintigraphy(dff), is_a("numeric") )
})
  