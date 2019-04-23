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
  
  expect_true(CumExpBeta(seq(0, 6, by = 0.5), 100, cf)[13] > 98.967)
  expect_error(CumExpBeta(seq(0, 6, by = 0.5), 100, df) ,"requires")
  
  expect_is(t50BluckCoward(cf), "numeric" )
  expect_is(t50BluckCoward(df), "numeric" )
  expect_is(t50BluckCoward(dff), "numeric" )
  
  expect_is(tLagBluckCoward(cf), "numeric" )
  expect_is(tLagBluckCoward(df), "numeric" )
  expect_is(tLagBluckCoward(dff), "numeric" )
  
  expect_is(t50Maes(cf), "numeric" )
  expect_is(t50Maes(df), "numeric" )
  expect_is(t50Maes(dff), "numeric" )
  
  expect_is(tLagMaes(cf), "numeric" )
  expect_is(tLagMaes(df), "numeric" )
  expect_is(tLagMaes(dff), "numeric" )
  
  expect_is(t50MaesScintigraphy(cf), "numeric")
  expect_is(t50MaesScintigraphy(df), "numeric")
  expect_is(t50MaesScintigraphy(dff), "numeric")
})
  