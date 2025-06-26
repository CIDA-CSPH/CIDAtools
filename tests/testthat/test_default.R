home_dir <- fs::path_home()
path <-fs::path_join(c(home_dir,"/cida_defaults.dcf"))


test_that("create temporary dcf file and test reading it", {
  dcf_content <- list( path="~/test_drive", analyst_name="Analyst Name")
  write.dcf(dcf_content, file.path(path))

  # Process the DCF file using your function
  default_data <- read_global_defaults()

  # Expectations
  expect_equal(default_data$path, "~/test_drive")
  expect_equal(default_data$analyst_name, "Analyst Name")


  # Clean up the temporary file
  #unlink(temp_dcf_file)
})

test_that("test getter functions", {

  default_data <- get_defaults()

  # Expectations
  expect_equal(default_data$path, "~/test_drive")
  expect_equal(default_data$analyst_name, "Analyst Name")

})

test_that("test getter functions", {

  analyst_name <- get_default_value("analyst_name")
  expect_equal(analyst_name, "Analyst Name")

  path <- get_global_default_path()
  analyst_name <- get_global_default_analyst()
  # Expectations
  expect_equal(path, "~/test_drive")
  expect_equal(analyst_name, "Analyst Name")


  expect_warning(non_existent <- get_default_value("name"),"Parameter:name does not exist in default values.")
  expect_equal(non_existent,'')

})

test_that("test setter functions",{
  set_global_default_analyst("Analyst Name2")
  set_global_default_path("~/test_drive2")
  path <- get_global_default_path()
  analyst_name <- get_global_default_analyst()
  # Expectations
  expect_equal(path, "~/test_drive2")
  expect_equal(analyst_name, "Analyst Name2")

})

test_that("test remove functions",{

})
