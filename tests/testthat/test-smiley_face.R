test_that("smiley basic test", {
  expect_is(object = smiley(), 
            class = c("gg", "ggplot"))
})
