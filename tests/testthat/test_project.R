home_dir <- fs::path_home()
test_drive_dir <- "~/test_drive/"
fs::dir_create(path=test_drive_dir)
test_project_path <- "Branches/dr_test/test_project"
full_project_dir <- paste(test_drive_dir,test_project_path,"/.ProjData/",sep="")
fs::dir_create(path= full_project_dir)

#test_that("test get project drive path", {
#  expect_error(path <- get_project_drive_path(),"Nothing found at /Volumes/dept || SPH || SPH-CIDA || CIDA Please ensure drive is mounted and you have entered your password to access the drive (and are logged into the VPN if needed.) If still experiencing issues try set_project_data_path() or set_global_default_path()")

  #w <- capture_warnings(path <- get_project_drive_path())
  #expect_match(w,)
  #expect_match(w, "\\.ProjData directory not found in project\\.", all = FALSE)
  #expect_match(w, "\\.ProjData directory not found in project\\.", all = FALSE)
  #expect_match(w,"\\.ProjData/Data.dcf File does not exist\\.", all=FALSE)
  #expect_match(w,"get_project_meta_data\\(default_full_path_to_project\\) returned NULL project data\\.", all=FALSE)

  #expect_equal(path,"")




#  })
