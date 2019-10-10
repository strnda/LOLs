test_that("logo basic test", {
  expect_is(object = logo(name = 'KVHEM'), 
            class = c("gg", "ggplot"))
})
