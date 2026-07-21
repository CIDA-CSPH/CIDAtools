test_that("test check_string_param_value", {
  expect_error(val<-check_string_param_value(1,"test") ,"test must be a character string")
  expect_warning(val<-check_string_param_value(c("string1","string2"),"test") ,"Only First String is Used")
})
