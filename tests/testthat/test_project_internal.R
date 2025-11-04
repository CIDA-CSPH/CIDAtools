home_dir <- fs::path_home()

global_path <-fs::path_join(c(home_dir,"/cida_defaults.dcf"))

test_drive_dir <- "~/test_drive"
fs::dir_create(path=test_drive_dir)
test_project_path <- "BRANCHES/dr_test/test_project5"
full_project_dir <- paste(test_drive_dir,test_project_path,"/.ProjData/",sep="")
fs::dir_create(path= full_project_dir)

test_that("test the get_default_path function",{
  w <- capture_warnings(test_path <- get_default_path())
  expect_equal(test_path,"")
  expect_match(w,".ProjData/Data.dcf File does not exist.",all=FALSE)
  expect_match(w,".ProjData directory not found in project.",all=FALSE)

  #Create Global Default File
  dcf_content <- list( path="~/test_drive", analyst_name="Analyst Name")
  write.dcf(dcf_content, file.path(global_path))
  w <- capture_warnings(test_path <- get_default_path())
  full_path_test <- as.character(fs::path_abs("~/test_drive"))
  expect_equal(test_path,full_path_test)

  #Create Project MetaData File
  setwd(paste(test_drive_dir,test_project_path,sep=""))
  dcf_content <- list( ProjectName="Test Project 5", PI="Dr. Test",analyst="Analyst Name",datalocation="Branches/dr_test/test_project5",gitlocation="CIDATools/Test_test_project5",default_full_path_to_project="~/test_drive/Branches/dr_test/test_project5")
  w <- capture_warnings(save_project_data(dcf_content))
  #expect_match(w,".ProjData/Data.dcf File does not exist.",all=FALSE)


  w <- capture_warnings(test_path <- get_default_path())
  full_path_test <- fs::path_abs("~/test_drive")
  expect_equal(test_path,full_path_test)


})


unlink(global_path)
