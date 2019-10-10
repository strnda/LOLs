test_that("mondrian basic test", {
  expect_is(object = mondRian(), 
            class = c("gg", "ggplot"))
})
